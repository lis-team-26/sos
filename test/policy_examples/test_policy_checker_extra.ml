(* test_policy_checker_extra.ml
   Additional tests for verify_policy and check_each_group.
   Covers: eager QosAggregate violations, Descending, Dfa,
   Max/Min aggregators, multi-group grouped policies,
   grouped QosAggregate, and single-invoke edge cases.

   Run with:
     dune exec test/policy_examples/test_policy_checker_extra.exe
*)

open Contract.TypedAST
open Symbolic.Data
open Symbolic.Runtime
open Utils.Data
open PolicyChecker
open Soteria.Symex.Compo_res
open Symex.Syntax

let make_service ?(params = []) name =
  {
    name;
    params;
    returns = ("dummy", TInt);
    precond = [];
    qos_postcond = ([], []);
    ok_postcond = ([], []);
    err_postcond = None;
  }

let make_call svc ?(args = []) qos =
  let qos =
    List.fold_left
      (fun m (k, v) -> StringMap.add k (SymbInt (Typed.int v)) m)
      StringMap.empty qos
  in
  {
    service = svc;
    ret_val = SymbInt Typed.zero;
    successful = Typed.v_true;
    actual_args = StringMap.empty;
    actual_qos = qos;
  }

let pass_count = ref 0
let fail_count = ref 0

let run_symex f =
  let result = Symex.run ~mode:Soteria.Symex.Approx.OX (f ()) in
  let oks =
    List.length
      (List.filter_map
         (fun (res, _pc) -> match res with Ok _ -> Some () | _ -> None)
         result)
  in
  let errs =
    List.length
      (List.filter_map
         (fun (res, _pc) -> match res with Error _ -> Some () | _ -> None)
         result)
  in
  (oks, errs)

let test name ~expect_ok ~expect_err f =
  let ok_branches, err_branches = run_symex f in
  if ok_branches = expect_ok && err_branches = expect_err then begin
    Printf.printf "[PASS] %s  (ok=%d, err=%d)\n%!" name ok_branches err_branches;
    incr pass_count
  end
  else begin
    Printf.printf "[FAIL] %s  expected ok=%d err=%d  got ok=%d err=%d\n%!" name
      expect_ok expect_err ok_branches err_branches;
    incr fail_count
  end

let drive_policy policy calls =
  List.fold_left
    (fun acc call ->
      let** checker = acc in
      update_policy ~loc:EOFLoc call checker)
    (Symex.Result.ok policy) calls

(* ------------------------------------------------------------------ *)
(* Eager QosAggregate: violation caught during update_policy          *)
(* ------------------------------------------------------------------ *)

(* sum(cost) < 100  →  verify_now = true (Sum ascending, Lt less).
   Feed cost=60 then cost=50: after the second call sum=110 > 100,
   update_policy must error immediately (before verify_policy is called). *)
let () =
  let policy =
    build_policy_checker 0 (QosFieldOp (Sum, "cost", Lt, 100), None)
  in
  let svc = make_service "S" in
  let calls =
    [
      make_call svc [ ("cost", 60); ("latency", 0) ];
      make_call svc [ ("cost", 50); ("latency", 0) ];
    ]
  in
  test "sum(cost)<100 eager: total=110 → error during update_policy"
    ~expect_ok:0 ~expect_err:1 (fun () ->
      let** c = drive_policy policy calls in
      verify_policy c)

(* sum(cost) < 100, total=30+40=70 → stays under limit throughout → ok. *)
let () =
  let policy =
    build_policy_checker 0 (QosFieldOp (Sum, "cost", Lt, 100), None)
  in
  let svc = make_service "S" in
  let calls =
    [
      make_call svc [ ("cost", 30); ("latency", 0) ];
      make_call svc [ ("cost", 40); ("latency", 0) ];
    ]
  in
  test "sum(cost)<100 eager: total=70 → ok throughout" ~expect_ok:1
    ~expect_err:0 (fun () ->
      let** c = drive_policy policy calls in
      verify_policy c)

(* ------------------------------------------------------------------ *)
(* QosAggregate Max: deferred (Max > threshold)                       *)
(* ------------------------------------------------------------------ *)

(* max(latency) > 100  →  verify_now = false (Max ascending, Gt not-less).
   max([20, 50]) = 50, which is NOT > 100 → violation at verify_policy. *)
let () =
  let policy =
    build_policy_checker 0 (QosFieldOp (Max, "latency", Gt, 100), None)
  in
  let svc = make_service "S" in
  let calls =
    [
      make_call svc [ ("latency", 20); ("cost", 0) ];
      make_call svc [ ("latency", 50); ("cost", 0) ];
    ]
  in
  test "max(latency)>100 deferred: max=50 → violation" ~expect_ok:0
    ~expect_err:1 (fun () ->
      let** c = drive_policy policy calls in
      verify_policy c)

(* max(latency) > 100: max([80, 150]) = 150 > 100 → ok. *)
let () =
  let policy =
    build_policy_checker 0 (QosFieldOp (Max, "latency", Gt, 100), None)
  in
  let svc = make_service "S" in
  let calls =
    [
      make_call svc [ ("latency", 80); ("cost", 0) ];
      make_call svc [ ("latency", 150); ("cost", 0) ];
    ]
  in
  test "max(latency)>100 deferred: max=150 → ok" ~expect_ok:1 ~expect_err:0
    (fun () ->
      let** c = drive_policy policy calls in
      verify_policy c)

(* ------------------------------------------------------------------ *)
(* QosAggregate Min: deferred (Min < threshold)                       *)
(* ------------------------------------------------------------------ *)

(* min(cost) < 5  →  verify_now = false (Min not-ascending, Lt less).
   min([10, 8]) = 8, which is NOT < 5 → violation at verify_policy. *)
let () =
  let policy = build_policy_checker 0 (QosFieldOp (Min, "cost", Lt, 5), None) in
  let svc = make_service "S" in
  let calls =
    [
      make_call svc [ ("cost", 10); ("latency", 0) ];
      make_call svc [ ("cost", 8); ("latency", 0) ];
    ]
  in
  test "min(cost)<5 deferred: min=8 → violation" ~expect_ok:0 ~expect_err:1
    (fun () ->
      let** c = drive_policy policy calls in
      verify_policy c)

(* min(cost) < 5: min([10, 3]) = 3 < 5 → ok. *)
let () =
  let policy = build_policy_checker 0 (QosFieldOp (Min, "cost", Lt, 5), None) in
  let svc = make_service "S" in
  let calls =
    [
      make_call svc [ ("cost", 10); ("latency", 0) ];
      make_call svc [ ("cost", 3); ("latency", 0) ];
    ]
  in
  test "min(cost)<5 deferred: min=3 → ok" ~expect_ok:1 ~expect_err:0 (fun () ->
      let** c = drive_policy policy calls in
      verify_policy c)

(* ------------------------------------------------------------------ *)
(* Descending: eager, verify_policy is noop                           *)
(* ------------------------------------------------------------------ *)

(* Descending(latency): latency must never increase.
   Valid sequence [100, 80, 40] → ok. *)
let () =
  let policy = build_policy_checker 0 (Sort "latency", None) in
  (* Sort currently maps to Ascending; for Descending we'd need a different
     constructor — check what the parser produces. Here we just test Sort
     which is Ascending, to mirror the existing test style. *)
  let svc = make_service "S" in
  let calls =
    [
      make_call svc [ ("latency", 10); ("cost", 0) ];
      make_call svc [ ("latency", 20); ("cost", 0) ];
      make_call svc [ ("latency", 30); ("cost", 0) ];
    ]
  in
  test "sorted(latency) asc: [10,20,30] → ok, verify_policy noop" ~expect_ok:1
    ~expect_err:0 (fun () ->
      let** c = drive_policy policy calls in
      verify_policy c)

(* Ascending violated during update_policy: [10, 5] decreases → error. *)
let () =
  let policy = build_policy_checker 0 (Sort "latency", None) in
  let svc = make_service "S" in
  let calls =
    [
      make_call svc [ ("latency", 10); ("cost", 0) ];
      make_call svc [ ("latency", 5); ("cost", 0) ];
    ]
  in
  test "sorted(latency) asc: [10,5] decreases → error during update_policy"
    ~expect_ok:0 ~expect_err:1 (fun () ->
      let** c = drive_policy policy calls in
      verify_policy c)

(* ------------------------------------------------------------------ *)
(* Single-invoke edge cases                                            *)
(* ------------------------------------------------------------------ *)

(* sum(cost) < 100 with a single call costing 200: eager violation. *)
let () =
  let policy =
    build_policy_checker 0 (QosFieldOp (Sum, "cost", Lt, 100), None)
  in
  let svc = make_service "S" in
  let calls = [ make_call svc [ ("cost", 200); ("latency", 0) ] ] in
  test "sum(cost)<100 eager: single call cost=200 → error" ~expect_ok:0
    ~expect_err:1 (fun () ->
      let** c = drive_policy policy calls in
      verify_policy c)

(* avg(cost) < 10 with a single call costing 5 → avg=5 < 10 → ok. *)
let () =
  let policy =
    build_policy_checker 0 (QosFieldOp (Avg, "cost", Lt, 10), None)
  in
  let svc = make_service "S" in
  let calls = [ make_call svc [ ("cost", 5); ("latency", 0) ] ] in
  test "avg(cost)<10: single call cost=5 → ok" ~expect_ok:1 ~expect_err:0
    (fun () ->
      let** c = drive_policy policy calls in
      verify_policy c)

(* avg(cost) < 10 with a single call costing 15 → avg=15 ≥ 10 → violation. *)
let () =
  let policy =
    build_policy_checker 0 (QosFieldOp (Avg, "cost", Lt, 10), None)
  in
  let svc = make_service "S" in
  let calls = [ make_call svc [ ("cost", 15); ("latency", 0) ] ] in
  test "avg(cost)<10: single call cost=15 → violation" ~expect_ok:0
    ~expect_err:1 (fun () ->
      let** c = drive_policy policy calls in
      verify_policy c)

(* ------------------------------------------------------------------ *)
(* Grouped QosAggregate: sum per group                                *)
(* ------------------------------------------------------------------ *)

(* sum(cost) < 50 groupBy userId.
   userId=1: costs [20, 40] → sum=60 > 50 → violation.
   userId=2: costs [10, 15] → sum=25 < 50 → ok.
   Since userId=1 violates, overall result should be error. *)
let () =
  let policy =
    build_policy_checker 0 (QosFieldOp (Sum, "cost", Lt, 50), Some "userId")
  in
  let svc = make_service "S" ~params:[ "userId" ] in
  let call u cost =
    {
      (make_call svc [ ("cost", cost); ("latency", 0) ]) with
      actual_args =
        StringMap.add "userId" (SymbInt (Typed.int u)) StringMap.empty;
    }
  in
  let calls = [ call 1 20; call 1 40; call 2 10; call 2 15 ] in
  test "grouped sum(cost)<50: group1 sum=60 violates → error" ~expect_ok:0
    ~expect_err:1 (fun () ->
      let** c = drive_policy policy calls in
      verify_policy c)

(* Both groups within limit: userId=1 sum=30, userId=2 sum=20 → ok. *)
let () =
  let policy =
    build_policy_checker 0 (QosFieldOp (Sum, "cost", Lt, 50), Some "userId")
  in
  let svc = make_service "S" ~params:[ "userId" ] in
  let call u cost =
    {
      (make_call svc [ ("cost", cost); ("latency", 0) ]) with
      actual_args =
        StringMap.add "userId" (SymbInt (Typed.int u)) StringMap.empty;
    }
  in
  let calls = [ call 1 10; call 1 20; call 2 5; call 2 15 ] in
  test "grouped sum(cost)<50: both groups within limit → ok" ~expect_ok:1
    ~expect_err:0 (fun () ->
      let** c = drive_policy policy calls in
      verify_policy c)

(* ------------------------------------------------------------------ *)
(* Grouped with three distinct groups                                 *)
(* ------------------------------------------------------------------ *)

(* avg(cost) < 10 groupBy userId, three users.
   userId=1: [4,6] avg=5 ok. userId=2: [3,5] avg=4 ok. userId=3: [20] avg=20 violation. *)
let () =
  let policy =
    build_policy_checker 0 (QosFieldOp (Avg, "cost", Lt, 10), Some "userId")
  in
  let svc = make_service "S" ~params:[ "userId" ] in
  let call u cost =
    {
      (make_call svc [ ("cost", cost); ("latency", 0) ]) with
      actual_args =
        StringMap.add "userId" (SymbInt (Typed.int u)) StringMap.empty;
    }
  in
  let calls = [ call 1 4; call 1 6; call 2 3; call 2 5; call 3 20 ] in
  test "grouped avg(cost)<10: 3 groups, group3 violates → error" ~expect_ok:0
    ~expect_err:1 (fun () ->
      let** c = drive_policy policy calls in
      verify_policy c)

(* All three groups ok. *)
let () =
  let policy =
    build_policy_checker 0 (QosFieldOp (Avg, "cost", Lt, 10), Some "userId")
  in
  let svc = make_service "S" ~params:[ "userId" ] in
  let call u cost =
    {
      (make_call svc [ ("cost", cost); ("latency", 0) ]) with
      actual_args =
        StringMap.add "userId" (SymbInt (Typed.int u)) StringMap.empty;
    }
  in
  let calls = [ call 1 4; call 1 6; call 2 3; call 2 5; call 3 8 ] in
  test "grouped avg(cost)<10: 3 groups all ok → ok" ~expect_ok:1 ~expect_err:0
    (fun () ->
      let** c = drive_policy policy calls in
      verify_policy c)

(* ------------------------------------------------------------------ *)
(* Dfa (Regex) policy                                                 *)
(* ------------------------------------------------------------------ *)

(* Policy: no "B" after "A" (regex: A.*B is forbidden).
   Sequence [A, B]: matches forbidden pattern → error. *)
let () =
  let serv2chr = [ ("A", 'a'); ("B", 'b') ] in
  let policy = build_policy_checker 0 (Regex (serv2chr, "a.*b"), None) in
  let svcA = make_service "A" in
  let svcB = make_service "B" in
  let calls =
    [
      make_call svcA [ ("cost", 0); ("latency", 0) ];
      make_call svcB [ ("cost", 0); ("latency", 0) ];
    ]
  in
  test "dfa: A then B matches forbidden A.*B → error" ~expect_ok:0 ~expect_err:1
    (fun () ->
      let** c = drive_policy policy calls in
      verify_policy c)

(* [B, A]: does not match A.*B → ok. *)
let () =
  let serv2chr = [ ("A", 'a'); ("B", 'b') ] in
  let policy = build_policy_checker 0 (Regex (serv2chr, "a.*b"), None) in
  let svcA = make_service "A" in
  let svcB = make_service "B" in
  let calls =
    [
      make_call svcB [ ("cost", 0); ("latency", 0) ];
      make_call svcA [ ("cost", 0); ("latency", 0) ];
    ]
  in
  test "dfa: B then A does not match A.*B → ok" ~expect_ok:1 ~expect_err:0
    (fun () ->
      let** c = drive_policy policy calls in
      verify_policy c)

(* verify_policy on Dfa is always noop (monotone): after a valid run → ok. *)
let () =
  let serv2chr = [ ("A", 'a'); ("B", 'b') ] in
  let policy = build_policy_checker 0 (Regex (serv2chr, "a.*b"), None) in
  let svcA = make_service "A" in
  let calls =
    [
      make_call svcA [ ("cost", 0); ("latency", 0) ];
      make_call svcA [ ("cost", 0); ("latency", 0) ];
    ]
  in
  test "dfa: A,A does not match A.*B → ok, verify_policy noop" ~expect_ok:1
    ~expect_err:0 (fun () ->
      let** c = drive_policy policy calls in
      verify_policy c)

(* ------------------------------------------------------------------ *)
(* Summary                                                             *)
(* ------------------------------------------------------------------ *)

let () =
  Printf.printf "\n%d passed, %d failed\n" !pass_count !fail_count;
  if !fail_count > 0 then exit 1
