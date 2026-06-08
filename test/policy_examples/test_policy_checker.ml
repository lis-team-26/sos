(* test_policy_checker.ml
   Unit tests for PolicyChecker.verify_policy and check_each_group.

   Run with:
     dune exec test/policy_examples/test_policy_checker.exe

   Each test calls init_policy, drives update_policy with a hand-built
   sequence of calls, then calls verify_policy and checks the outcome.
   No orchestrator or symbolic interpreter involved.
*)

module PC   = PolicyChecker
module Symex = PC.Symex
module Typed = Soteria.Tiny_values.Typed
module StrMap = Map.Make (String)

(* ------------------------------------------------------------------ *)
(* Helpers to build Contract.AST values without a parser              *)
(* ------------------------------------------------------------------ *)

(** Minimal service record; only [name] and [params] matter for
    [update_policy] (used when groupBy looks up a parameter index). *)
let make_service ?(params = []) name : Contract.AST.service =
  { name
  ; params
  ; returns    = []
  ; trust      = 0
  ; precond    = []
  ; qos        = ([], [])
  ; ok_post    = ([], [])
  ; err_post   = ([], [])
  }

(** Build a [PC.call] with symbolic-integer QoS values constructed from
    concrete OCaml ints. *)
let make_call serv_name ?(args = []) qos_pairs : PC.call =
  let qos =
    List.fold_left
      (fun m (k, v) -> StrMap.add k (Typed.int v) m)
      StrMap.empty
      qos_pairs
  in
  { PC.serv_name; args = List.map Typed.int args; qos }

(** Build a service map from a list of [Contract.AST.service] values. *)
let service_map services =
  List.fold_left
    (fun m (s : Contract.AST.service) -> StrMap.add s.name s m)
    StrMap.empty
    services

(* ------------------------------------------------------------------ *)
(* Small test framework                                                *)
(* ------------------------------------------------------------------ *)

let pass_count = ref 0
let fail_count = ref 0

(** Run [f ()] under the Soteria symbolic engine and collect results.
    Returns [(ok_count, err_count)] where ok_count is the number of
    branches that ended in [ok] and err_count those that ended in [error]. *)
let run_symex (f : unit -> (unit, string, 'c) Symex.Result.t) =
  let result = Symex.run ~mode:Soteria.Symex.Approx.OX (f ()) in
  let oks  = List.length (List.filter_map (fun (res, _pc) -> match res with
      | Soteria.Symex.Compo_res.Ok _  -> Some ()
      | _ -> None) result) in
  let errs = List.length (List.filter_map (fun (res, _pc) -> match res with
      | Soteria.Symex.Compo_res.Error _ -> Some ()
      | _ -> None) result) in
  (oks, errs)

let test name ~expect_ok ~expect_err f =
  let (ok_branches, err_branches) = run_symex f in
  if ok_branches = expect_ok && err_branches = expect_err then begin
    Printf.printf "[PASS] %s  (ok=%d, err=%d)\n%!" name ok_branches err_branches;
    incr pass_count
  end else begin
    Printf.printf "[FAIL] %s  expected ok=%d err=%d  got ok=%d err=%d\n%!"
      name expect_ok expect_err ok_branches err_branches;
    incr fail_count
  end

(* ------------------------------------------------------------------ *)
(* Shared Symex syntax (copied from policyChecker.ml style)           *)
(* ------------------------------------------------------------------ *)

open Symex.Syntax

(* ------------------------------------------------------------------ *)
(* Helper: drive a policy through a sequence of calls, return checker *)
(* ------------------------------------------------------------------ *)

let drive_policy policy calls servMap =
  List.fold_left
    (fun acc_m call ->
      let** checker = acc_m in
      PC.update_policy servMap call checker)
    (Symex.Result.ok policy)
    calls

(* ------------------------------------------------------------------ *)
(* Tests                                                               *)
(* ------------------------------------------------------------------ *)

(* ---- verify_policy: QosAggregate with verNow = false -------------- *)

(** sum(latency) > 5  -->  verNow = false (sum is ascending, > is ascending).
    Actually verify_now(Sum, Gt) = (ascending Sum = true) == (less Gt = false) = false.
    So the check is deferred to verify_policy.

    Feed two calls each with latency=1 → total=2, which is NOT > 5
    → verify_policy should produce an error branch. *)
let test_qos_aggregate_deferred_violation () =
  let policy_def = (Contract.AST.QosFieldOp (Contract.AST.Sum, "latency", Contract.AST.Gt, 5), None) in
  let checker    = PC.init_policy policy_def in
  let svc        = make_service "Svc" in
  let smap       = service_map [svc] in
  let calls      = [ make_call "Svc" [("latency", 1); ("cost", 0)]
                   ; make_call "Svc" [("latency", 1); ("cost", 0)] ] in
      let** checker = drive_policy checker calls smap in
    PC.verify_policy checker

let () = test "sum(latency)>5 deferred: total=2 → violation"
    ~expect_ok:0 ~expect_err:1
    test_qos_aggregate_deferred_violation

(** Same policy, but feed latency=3+3=6 → total=6 > 5 → ok. *)
let test_qos_aggregate_deferred_ok () =
  let policy_def = (Contract.AST.QosFieldOp (Contract.AST.Sum, "latency", Contract.AST.Gt, 5), None) in
  let checker    = PC.init_policy policy_def in
  let svc        = make_service "Svc" in
  let smap       = service_map [svc] in
  let calls      = [ make_call "Svc" [("latency", 3); ("cost", 0)]
                   ; make_call "Svc" [("latency", 3); ("cost", 0)] ] in
      let** checker = drive_policy checker calls smap in
    PC.verify_policy checker

let () = test "sum(latency)>5 deferred: total=6 → ok"
    ~expect_ok:1 ~expect_err:0
    test_qos_aggregate_deferred_ok

(* ---- verify_policy: QosAvg always deferred ----------------------- *)

(** avg(cost) < 10.  Two calls: cost=5 and cost=7 → avg=6 < 10 → ok. *)
let test_avg_ok () =
  let policy_def = (Contract.AST.QosFieldOp (Contract.AST.Avg, "cost", Contract.AST.Lt, 10), None) in
  let checker    = PC.init_policy policy_def in
  let svc        = make_service "Svc" in
  let smap       = service_map [svc] in
  let calls      = [ make_call "Svc" [("latency", 0); ("cost", 5)]
                   ; make_call "Svc" [("latency", 0); ("cost", 7)] ] in
      let** checker = drive_policy checker calls smap in
    PC.verify_policy checker

let () = test "avg(cost)<10: avg=6 → ok"
    ~expect_ok:1 ~expect_err:0
    test_avg_ok

(** avg(cost) < 10.  Two calls: cost=12 and cost=14 → avg=13 ≥ 10 → violation. *)
let test_avg_violation () =
  let policy_def = (Contract.AST.QosFieldOp (Contract.AST.Avg, "cost", Contract.AST.Lt, 10), None) in
  let checker    = PC.init_policy policy_def in
  let svc        = make_service "Svc" in
  let smap       = service_map [svc] in
  let calls      = [ make_call "Svc" [("latency", 0); ("cost", 12)]
                   ; make_call "Svc" [("latency", 0); ("cost", 14)] ] in
      let** checker = drive_policy checker calls smap in
    PC.verify_policy checker

let () = test "avg(cost)<10: avg=13 → violation"
    ~expect_ok:0 ~expect_err:1
    test_avg_violation

(** avg(cost) < 10.  No calls → count=0 → verify_policy must return ok
    (the guard [if count = 0 then ok] branch). *)
let test_avg_empty_history () =
  let policy_def = (Contract.AST.QosFieldOp (Contract.AST.Avg, "cost", Contract.AST.Lt, 10), None) in
  let checker    = PC.init_policy policy_def in
  PC.verify_policy checker

let () = test "avg(cost)<10: empty history → ok (no division by zero)"
    ~expect_ok:1 ~expect_err:0
    test_avg_empty_history

(* ---- verify_policy: Ascending/Descending are eager → always ok at end *)

(** sorted(cost) ascending: already checked at every update_policy call.
    At verify_policy time there is nothing extra to check → always ok. *)
let test_ascending_verify_noop () =
  let policy_def = (Contract.AST.Sort "cost", None) in
  let checker    = PC.init_policy policy_def in
  let svc        = make_service "Svc" in
  let smap       = service_map [svc] in
  (* A descending sequence would have already errored during update_policy;
     here we pass a valid ascending sequence so we reach verify_policy. *)
  let calls      = [ make_call "Svc" [("cost", 1); ("latency", 0)]
                   ; make_call "Svc" [("cost", 3); ("latency", 0)]
                   ; make_call "Svc" [("cost", 7); ("latency", 0)] ] in
      let** checker = drive_policy checker calls smap in
    PC.verify_policy checker

let () = test "sorted(cost) asc: valid seq → verify_policy is noop (ok)"
    ~expect_ok:1 ~expect_err:0
    test_ascending_verify_noop

(* ---- check_each_group: grouped avg, two distinct keys ------------- *)

(** avg(cost) < 10 groupBy userId.
    Group userId=1: costs [4, 6] → avg=5 < 10 → ok.
    Group userId=2: costs [20]   → avg=20 ≥ 10 → violation.
    Expected: verify_policy must check BOTH groups and report the error
    from group userId=2, even though group userId=1 is fine.

    This is the exact scenario Andrea described: check_each_group must
    iterate over all keys in the ValMap, not just one. *)
let test_grouped_avg_one_group_violates () =
  let policy_def =
    ( Contract.AST.QosFieldOp (Contract.AST.Avg, "cost", Contract.AST.Lt, 10)
    , Some "userId" )
  in
  let checker = PC.init_policy policy_def in
  (* Service has a parameter named "userId" at index 0 *)
  let svc  = make_service "Svc" ~params:[("userId", Contract.AST.TInt)] in
  let smap = service_map [svc] in
  (* userId=1 calls (index 0 → arg value 1) *)
  let call_u1_cost4  = { (make_call "Svc" [("cost", 4);  ("latency", 0)]) with
                          PC.args = [Typed.int 1] } in
  let call_u1_cost6  = { (make_call "Svc" [("cost", 6);  ("latency", 0)]) with
                          PC.args = [Typed.int 1] } in
  (* userId=2 call *)
  let call_u2_cost20 = { (make_call "Svc" [("cost", 20); ("latency", 0)]) with
                          PC.args = [Typed.int 2] } in
  let calls = [call_u1_cost4; call_u1_cost6; call_u2_cost20] in
      let** checker = drive_policy checker calls smap in
    PC.verify_policy checker

let () = test "grouped avg(cost)<10: group1 ok, group2 violates → error"
    ~expect_ok:0 ~expect_err:1
    test_grouped_avg_one_group_violates

(** Same but both groups are ok. *)
let test_grouped_avg_both_ok () =
  let policy_def =
    ( Contract.AST.QosFieldOp (Contract.AST.Avg, "cost", Contract.AST.Lt, 10)
    , Some "userId" )
  in
  let checker = PC.init_policy policy_def in
  let svc  = make_service "Svc" ~params:[("userId", Contract.AST.TInt)] in
  let smap = service_map [svc] in
  let call u cost = { (make_call "Svc" [("cost", cost); ("latency", 0)]) with
                       PC.args = [Typed.int u] } in
  let calls = [call 1 4; call 1 6; call 2 3; call 2 5] in
      let** checker = drive_policy checker calls smap in
    PC.verify_policy checker

let () = test "grouped avg(cost)<10: both groups ok → ok"
    ~expect_ok:1 ~expect_err:0
    test_grouped_avg_both_ok

(* ---- check_each_group: service without the groupBy param is skipped *)

(** avg(cost) < 10 groupBy userId.
    If a service does NOT have the "userId" parameter, update_policy
    must skip it entirely (no group is created for it).
    verify_policy on an empty ValMap must return ok. *)
let test_grouped_skips_service_without_param () =
  let policy_def =
    ( Contract.AST.QosFieldOp (Contract.AST.Avg, "cost", Contract.AST.Lt, 10)
    , Some "userId" )
  in
  let checker = PC.init_policy policy_def in
  (* Service with NO "userId" parameter *)
  let svc  = make_service "OtherSvc" ~params:[("x", Contract.AST.TInt)] in
  let smap = service_map [svc] in
  let calls = [ make_call "OtherSvc" [("cost", 50); ("latency", 0)]
              ; make_call "OtherSvc" [("cost", 50); ("latency", 0)] ] in
      let** checker = drive_policy checker calls smap in
    (* All invocations were skipped by map_state, so ValMap is empty
       → verify_policy must not error *)
    PC.verify_policy checker

let () = test "grouped: service without groupBy param is skipped → ok"
    ~expect_ok:1 ~expect_err:0
    test_grouped_skips_service_without_param

(* ---- summary -------------------------------------------------------- *)

(* ------------------------------------------------------------------ *)
(* Bad prefix tests *)
(* ------------------------------------------------------------------ *)
module Bad_pref_test = struct
let () =

  let make_smap services = make_smap @@ List.map make_service services in
  let make_history services = List.map (fun svc -> make_call svc []) services in
  let make_policy serv2letter reg =
    PC.init_policy (Contract.AST.Regex (serv2letter, reg), None)
  in
  let make_thunk serv2letter reg services calls =
    let smap = make_smap services in
    let call_history = make_history calls in
    fun () ->
    let** policy = Symex.Result.ok (make_policy serv2letter reg) in
    let** c = drive policy call_history smap in
    PC.verify_policy c 
  in
  let fbd = make_thunk  ["Forbidden",'f';"Other",'o'] "[f-o]*f"  ["Other"; "Ignore"; "Forbidden"] in 
  test "never call forbidden: valid seq → ok"
    ~expect_ok:1 ~expect_err:0
     (fbd ["Other"; "Ignore"]);
  test "never call forbidden:  invalid seq → error"
   ~expect_ok:0 ~expect_err:1
   (fbd ["Other"; "Ignore"; "Forbidden"; "Other" ; "Ignore"]);
  let up_after_read = make_thunk ["Other",'o';"Read",'r';"Update",'u'] ".*ru" 
                       [ "Other"; "Read"
                       ; "Update"; "Ignore" ] 
  in
  test "no update after read: valid seq → ok"
    ~expect_ok:1 ~expect_err:0
    (up_after_read  [ "Other"; "Update"; "Read"; "Ignore"]);
  test "no update after read: invalid seq → error"
    ~expect_ok:0 ~expect_err:1
    (up_after_read ["Other"; "Update"; "Ignore"; "Read";  
                   "Update"; "Read" ]);
end    
    
let () =
  Printf.printf "\n%d passed, %d failed\n" !pass_count !fail_count;
  if !fail_count > 0 then exit 1
