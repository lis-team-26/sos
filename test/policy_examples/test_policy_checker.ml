(* test_policy_checker.ml
   Tests for verify_policy and check_each_group.

   Run with:
     dune exec test/policy_checker_tests/test_policy_checker.exe
*)

module PC    = PolicyChecker
module Symex = PolicyChecker.Symex
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
(* Test runner *)
(* ------------------------------------------------------------------ *)

let pass_count = ref 0
let fail_count = ref 0

let run_symex f =
  (* Invece di passare (f ()), passiamo f direttamente *)
  let results = Symex.run ~mode:Soteria.Symex.Approx.OX (f ()) in
  List.fold_left (fun (ok, err) (res, _pc) ->
    match res with
    | Soteria.Symex.Compo_res.Ok    _ -> (ok + 1, err)
    | Soteria.Symex.Compo_res.Error _ -> (ok, err + 1)
    | _ -> (ok, err)
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

let drive checker calls smap =
  List.fold_left (fun acc call ->
    let** c = acc in
    PC.update_policy smap call c)
  (Symex.Result.ok checker) calls

(* ------------------------------------------------------------------ *)
(* Tests                                                              *)
(* ------------------------------------------------------------------ *)

let () =
  test "sum(latency)>5 deferred: total=2 → violation" ~expect_ok:0 ~expect_err:1
    (fun () ->
       let** policy = Symex.Result.ok (PC.init_policy (Contract.AST.QosFieldOp (Sum, "latency", Gt, 5), None)) in
       let svc   = make_service "S" in
       let smap  = make_smap [svc] in
       let calls = [ make_call "S" [("latency", 1); ("cost", 0)]
                   ; make_call "S" [("latency", 1); ("cost", 0)] ] in
       let** c = drive policy calls smap in
       PC.verify_policy c)

let () =
  test "sum(latency)>5 deferred: total=6 → ok" ~expect_ok:1 ~expect_err:0
    (fun () ->
       let** policy = Symex.Result.ok (PC.init_policy (Contract.AST.QosFieldOp (Sum, "latency", Gt, 5), None)) in
       let svc   = make_service "S" in
       let smap  = make_smap [svc] in
       let calls = [ make_call "S" [("latency", 3); ("cost", 0)]
                   ; make_call "S" [("latency", 3); ("cost", 0)] ] in
       let** c = drive policy calls smap in
       PC.verify_policy c)

let () =
  test "avg(cost)<10: costs 5,7 → ok throughout" ~expect_ok:1 ~expect_err:0
    (fun () ->
       let** policy = Symex.Result.ok (PC.init_policy (Contract.AST.QosFieldOp (Avg, "cost", Lt, 10), None)) in
       let svc   = make_service "S" in
       let smap  = make_smap [svc] in
       let calls = [ make_call "S" [("cost", 5); ("latency", 0)]
                   ; make_call "S" [("cost", 7); ("latency", 0)] ] in
       let** c = drive policy calls smap in
       PC.verify_policy c)

let () =
  test "avg(cost)<10: empty history → ok (no div/0)" ~expect_ok:1 ~expect_err:0
    (fun () -> 
       let** policy = Symex.Result.ok (PC.init_policy (Contract.AST.QosFieldOp (Avg, "cost", Lt, 10), None)) in
       PC.verify_policy policy)

let () =
  test "sum(cost)<100 eager: valid seq → verify_policy noop (ok)"
    ~expect_ok:1 ~expect_err:0
    (fun () ->
       let** policy = Symex.Result.ok (PC.init_policy (Contract.AST.QosFieldOp (Sum, "cost", Lt, 100), None)) in
       let svc   = make_service "S" in
       let smap  = make_smap [svc] in
       let calls = [ make_call "S" [("cost", 10); ("latency", 0)]
                   ; make_call "S" [("cost", 20); ("latency", 0)] ] in
       let** c = drive policy calls smap in
       PC.verify_policy c)

let () =
  test "Ascending(cost): valid seq → verify_policy noop (ok)"
    ~expect_ok:1 ~expect_err:0
    (fun () ->
       let** policy = Symex.Result.ok (PC.init_policy (Contract.AST.Sort "cost", None)) in
       let svc   = make_service "S" in
       let smap  = make_smap [svc] in
       let calls = [ make_call "S" [("cost", 1); ("latency", 0)]
                   ; make_call "S" [("cost", 5); ("latency", 0)]
                   ; make_call "S" [("cost", 9); ("latency", 0)] ] in
       let** c = drive policy calls smap in
       PC.verify_policy c)

let () =
  test "grouped avg(cost)<10: group1 ok, group2 violates → error"
    ~expect_ok:0 ~expect_err:1
    (fun () ->
       let** policy = Symex.Result.ok (PC.init_policy (Contract.AST.QosFieldOp (Avg, "cost", Lt, 10), Some "userId")) in
       let svc  = make_service "S" ~params:[("userId", Contract.AST.TInt)] in
       let smap = make_smap [svc] in
       let call u cost =
         { (make_call "S" [("cost", cost); ("latency", 0)]) with
           PC.args = [Typed.int u] } in
       let calls = [call 1 4; call 1 6; call 2 12] in
       let** c = drive policy calls smap in
       PC.verify_policy c)

let () =
  test "grouped avg(cost)<10: both groups ok → ok"
    ~expect_ok:1 ~expect_err:0
    (fun () ->
       let** policy = Symex.Result.ok (PC.init_policy (Contract.AST.QosFieldOp (Avg, "cost", Lt, 10), Some "userId")) in
       let svc  = make_service "S" ~params:[("userId", Contract.AST.TInt)] in
       let smap = make_smap [svc] in
       let call u cost =
         { (make_call "S" [("cost", cost); ("latency", 0)]) with
           PC.args = [Typed.int u] } in
       let calls = [call 1 4; call 1 6; call 2 3; call 2 5] in
       let** c = drive policy calls smap in
       PC.verify_policy c)

let () =
  test "grouped: service without groupBy param → skipped → ok"
    ~expect_ok:1 ~expect_err:0
    (fun () ->
       let** policy = Symex.Result.ok (PC.init_policy (Contract.AST.QosFieldOp (Avg, "cost", Lt, 10), Some "userId")) in
       let svc  = make_service "Other" ~params:[("x", Contract.AST.TInt)] in
       let smap = make_smap [svc] in
       let calls = [ make_call "Other" [("cost", 50); ("latency", 0)]
                   ; make_call "Other" [("cost", 50); ("latency", 0)] ] in
       let** c = drive policy calls smap in
       PC.verify_policy c)

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
