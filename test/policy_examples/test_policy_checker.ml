(* test_policy_checker.ml
   Tests for verify_policy and check_each_group.

   Run with:
     dune exec test/policy_checker_tests/test_policy_checker.exe
*)

module PC    = PolicyChecker
module Symex = Soteria.Symex.Make (Soteria.Tiny_values.Tiny_solver.Z3_solver)
module Typed = Soteria.Tiny_values.Typed
module StrMap = Map.Make (String)
open Symex.Syntax

(* ------------------------------------------------------------------ *)
(* Helpers                                                             *)
(* ------------------------------------------------------------------ *)

let make_service ?(params = []) name : Contract.AST.service =
  { name; params; returns = []; trust = 0; precond = [];
    qos = ([], []); ok_post = ([], []); err_post = ([], []) }

(** Build a PC.call.  [qos_pairs] uses string keys like "cost"/"latency";
    values are concrete ints wrapped as symbolic. *)
let make_call serv_name ?(args = []) qos_pairs : PC.call =
  let qos =
    List.fold_left (fun m (k, v) -> StrMap.add k (Typed.int v) m)
      StrMap.empty qos_pairs
  in
  { PC.serv_name; args = List.map Typed.int args; qos }

let make_smap services =
  List.fold_left (fun m (s : Contract.AST.service) -> StrMap.add s.name s m)
    StrMap.empty services

(* ------------------------------------------------------------------ *)
(* Test runner
   Symex.run returns  (Compo_res.t * path_cond list) list.
   We count ok/error branches by inspecting the first component.       *)
(* ------------------------------------------------------------------ *)

let pass_count = ref 0
let fail_count = ref 0

let run_symex f =
  let results = Symex.run ~mode:Soteria.Symex.Approx.OX (f ()) in
  List.fold_left (fun (ok, err) (res, _pc) ->
    match res with
    | Soteria.Symex.Compo_res.Ok    _ -> (ok + 1, err)
    | Soteria.Symex.Compo_res.Error _ -> (ok, err + 1)
    | _ -> (ok, err)  (* ignore other results, e.g. Incomplete *)
  ) (0, 0) results

let test name ~expect_ok ~expect_err f =
  let (ok, err) = run_symex f in
  if ok = expect_ok && err = expect_err then begin
    Printf.printf "[PASS] %s  (ok=%d err=%d)\n%!" name ok err;
    incr pass_count
  end else begin
    Printf.printf "[FAIL] %s  expected ok=%d err=%d  got ok=%d err=%d\n%!"
      name expect_ok expect_err ok err;
    incr fail_count
  end

(* Convenience: drive a checker through a list of calls *)
let drive checker calls smap =
  List.fold_left (fun acc call ->
    let** c = acc in
    PC.update_policy smap call c)
  (Symex.Result.ok checker) calls

(* ------------------------------------------------------------------ *)
(* Tests: QosAggregate with verNow = false                            *)
(* ------------------------------------------------------------------ *)

(* sum(latency) > 5  →  verify_now = false (ascending, not less-than)
   Two calls with latency=1 each → total symbolic sum = 2.
   At verify_policy: 2 > 5 is false → violation. *)
let () =
  let policy = PC.init_policy (Contract.AST.QosFieldOp (Sum, "latency", Gt, 5), None) in
  let svc   = make_service "S" in
  let smap  = make_smap [svc] in
  let calls = [ make_call "S" [("latency", 1); ("cost", 0)]
              ; make_call "S" [("latency", 1); ("cost", 0)] ] in
  test "sum(latency)>5 deferred: total=2 → violation" ~expect_ok:0 ~expect_err:1
    (fun () ->
       let** c = drive policy calls smap in
       PC.verify_policy c)

(* Same policy, latency=3+3=6 → 6 > 5 ok. *)
let () =
  let policy = PC.init_policy (Contract.AST.QosFieldOp (Sum, "latency", Gt, 5), None) in
  let svc   = make_service "S" in
  let smap  = make_smap [svc] in
  let calls = [ make_call "S" [("latency", 3); ("cost", 0)]
              ; make_call "S" [("latency", 3); ("cost", 0)] ] in
  test "sum(latency)>5 deferred: total=6 → ok" ~expect_ok:1 ~expect_err:0
    (fun () ->
       let** c = drive policy calls smap in
       PC.verify_policy c)

(* ------------------------------------------------------------------ *)
(* Tests: QosAvg (always deferred)                                    *)
(* ------------------------------------------------------------------ *)

(* avg(cost) < 10 — note: update_policy for QosAvg already checks eagerly,
   so we test verify_policy on a fresh checker fed with concrete values. *)
let () =
  let policy = PC.init_policy (Contract.AST.QosFieldOp (Avg, "cost", Lt, 10), None) in
  let svc   = make_service "S" in
  let smap  = make_smap [svc] in
  (* cost = 5, 7  →  each step avg check: after 1st call avg=5 ok,
     after 2nd call avg≈6 ok. verify_policy also ok. *)
  let calls = [ make_call "S" [("cost", 5); ("latency", 0)]
              ; make_call "S" [("cost", 7); ("latency", 0)] ] in
  test "avg(cost)<10: costs 5,7 → ok throughout" ~expect_ok:1 ~expect_err:0
    (fun () ->
       let** c = drive policy calls smap in
       PC.verify_policy c)

(* avg(cost) < 10 — empty history → count=0 → verify_policy must not
   divide by zero and must return ok. *)
let () =
  let policy = PC.init_policy (Contract.AST.QosFieldOp (Avg, "cost", Lt, 10), None) in
  test "avg(cost)<10: empty history → ok (no div/0)" ~expect_ok:1 ~expect_err:0
    (fun () -> PC.verify_policy policy)

(* ------------------------------------------------------------------ *)
(* Tests: eager policies → verify_policy is a no-op                  *)
(* ------------------------------------------------------------------ *)

(* sum(cost) < 100  →  verify_now = true (ascending Sum, less-than Lt).
   Feed valid calls; verify_policy must return ok (nothing extra to check). *)
let () =
  let policy = PC.init_policy (Contract.AST.QosFieldOp (Sum, "cost", Lt, 100), None) in
  let svc   = make_service "S" in
  let smap  = make_smap [svc] in
  let calls = [ make_call "S" [("cost", 10); ("latency", 0)]
              ; make_call "S" [("cost", 20); ("latency", 0)] ] in
  test "sum(cost)<100 eager: valid seq → verify_policy noop (ok)"
    ~expect_ok:1 ~expect_err:0
    (fun () ->
       let** c = drive policy calls smap in
       PC.verify_policy c)

(* Ascending: already checked eagerly; verify_policy is noop. *)
let () =
  let policy = PC.init_policy (Contract.AST.Sort "cost", None) in
  let svc   = make_service "S" in
  let smap  = make_smap [svc] in
  let calls = [ make_call "S" [("cost", 1); ("latency", 0)]
              ; make_call "S" [("cost", 5); ("latency", 0)]
              ; make_call "S" [("cost", 9); ("latency", 0)] ] in
  test "Ascending(cost): valid seq → verify_policy noop (ok)"
    ~expect_ok:1 ~expect_err:0
    (fun () ->
       let** c = drive policy calls smap in
       PC.verify_policy c)

(* ------------------------------------------------------------------ *)
(* Tests: grouped policies — check_each_group                        *)
(* ------------------------------------------------------------------ *)

(* avg(cost)<10 groupBy userId.
   userId=1: costs [4, 6] → avg=5 < 10 → ok.
   userId=2: costs [12]   → avg=12 ≥ 10 → violation.
   verify_policy must check BOTH groups; error expected because of group 2.
   This is the exact case raised by Andrea: check_each_group must iterate
   over ALL keys in the ValMap, not just one. *)
let () =
  let policy = PC.init_policy
      (Contract.AST.QosFieldOp (Avg, "cost", Lt, 10), Some "userId") in
  let svc  = make_service "S" ~params:[("userId", Contract.AST.TInt)] in
  let smap = make_smap [svc] in
  (* We set args=[userId_value] so map_state can find index 0 = userId *)
  let call u cost =
    { (make_call "S" [("cost", cost); ("latency", 0)]) with
      PC.args = [Typed.int u] } in
  let calls = [call 1 4; call 1 6; call 2 12] in
  test "grouped avg(cost)<10: group1 ok, group2 violates → error"
    ~expect_ok:0 ~expect_err:1
    (fun () ->
       let** c = drive policy calls smap in
       PC.verify_policy c)

(* Both groups ok. *)
let () =
  let policy = PC.init_policy
      (Contract.AST.QosFieldOp (Avg, "cost", Lt, 10), Some "userId") in
  let svc  = make_service "S" ~params:[("userId", Contract.AST.TInt)] in
  let smap = make_smap [svc] in
  let call u cost =
    { (make_call "S" [("cost", cost); ("latency", 0)]) with
      PC.args = [Typed.int u] } in
  let calls = [call 1 4; call 1 6; call 2 3; call 2 5] in
  test "grouped avg(cost)<10: both groups ok → ok"
    ~expect_ok:1 ~expect_err:0
    (fun () ->
       let** c = drive policy calls smap in
       PC.verify_policy c)

(* Service without the groupBy param is skipped entirely.
   ValMap stays empty; verify_policy on empty Grouped state must return ok. *)
let () =
  let policy = PC.init_policy
      (Contract.AST.QosFieldOp (Avg, "cost", Lt, 10), Some "userId") in
  let svc  = make_service "Other" ~params:[("x", Contract.AST.TInt)] in
  let smap = make_smap [svc] in
  let calls = [ make_call "Other" [("cost", 50); ("latency", 0)]
              ; make_call "Other" [("cost", 50); ("latency", 0)] ] in
  test "grouped: service without groupBy param → skipped → ok"
    ~expect_ok:1 ~expect_err:0
    (fun () ->
       let** c = drive policy calls smap in
       PC.verify_policy c)

(* ------------------------------------------------------------------ *)
(* Summary                                                             *)
(* ------------------------------------------------------------------ *)

let () =
  Printf.printf "\n%d passed, %d failed\n" !pass_count !fail_count;
  if !fail_count > 0 then exit 1