module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

type ident = string

type 'a env = 'a StringMap.t
(** A mapping from variable names to their values *)

type 'a scope_stack = 'a env list
(** A stack of environments representing nested scopes. The head of the list is
    the innermost scope, and the tail represents outer scopes. *)

(** Looks up a variable in the scope stack, starting from the innermost scope.
    Returns None if the variable is not found in any of the scopes *)
let rec lookup x = function
  | [] -> None
  | env :: rest -> (
      match StringMap.find_opt x env with
      | Some v -> Some v
      | None -> lookup x rest)

(** Updates the value of a variable in the scope stack. Fails if the variable is
    not found in any of the environments. *)
let rec update x v = function
  | [] -> failwith (Fmt.str "Variable %s not declared" x)
  | env :: rest ->
      if StringMap.mem x env then StringMap.add x v env :: rest
      else env :: update x v rest

(** Introduces [x |-> v] in the innermost scope, shadowing any binding of [x] in
    an outer scope (or overwriting it in the innermost one). Starts a new
    innermost scope if the stack is empty. Unlike {!update}, which rewrites an
    existing binding wherever it lives, this always binds in the head scope —
    the behaviour a block-scoped declaration needs. *)
let declare x v = function
  | [] -> failwith "Empty scope stack"
  | env :: rest -> StringMap.add x v env :: rest

(** Adds a new variable to the innermost scope. Fails if the scope stack is
    empty. *)
let push_scope s = StringMap.empty :: s

(** Removes the innermost scope from the scope stack. Returns None if the scope
    stack is empty. *)
let pop_scope s =
  match s with [] -> failwith "Scope stack is empty" | _ :: rest -> rest

let rec get_public_env = function
  | [] -> failwith "Scope stack is empty"
  | [ env ] -> env
  | _ :: rest -> get_public_env rest

let rec set_public_env env = function
  | [] -> failwith "Scope stack is empty"
  | [ _ ] -> [ env ]
  | env :: rest -> env :: set_public_env env rest

let get_result pp = function
  | Ok v -> v
  | Error msg -> failwith (Fmt.str "%a" pp msg)

(* Sequences a list of results into a result of a list, short-circuiting on the
   first error. *)
let rec sequence_results = function
  | [] -> Ok []
  | Error err :: _ -> Error err
  | Ok x :: rest -> (
      match sequence_results rest with
      | Ok xs -> Ok (x :: xs)
      | Error err -> Error err)
