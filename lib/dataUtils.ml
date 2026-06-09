module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

type ident = string

type 'a env = 'a StringMap.t
(** A mapping from variable names to their values *)

type 'a scope_stack = 'a env list
(** A stack of environments representing nested scopes. The head of the list is
    the innermost scope, and the tail represents outer scopes. *)

(** Builds an environment from an association list. Later bindings for the same
    key override earlier ones. *)
let env_of_list bindings =
  List.fold_left
    (fun acc (x, v) -> StringMap.add x v acc)
    StringMap.empty bindings

(** Looks up a variable in the scope stack, starting from the innermost scope.
    Returns None if the variable is not found in any of the scopes *)
let rec lookup x = function
  | [] -> None
  | env :: rest -> (
      match StringMap.find_opt x env with
      | Some v -> Some v
      | None -> lookup x rest)

(** Updates the value of a variable in the scope stack. If the variable is not
    found in any of the environments, it adds it to the innermost environment.
    Fails if the scope stack is empty *)
let rec update x v s =
  let rec helper = function
    | [] -> None
    | env :: rest -> (
        if StringMap.mem x env then Some (StringMap.add x v env :: rest)
        else
          match helper rest with
          | Some updated_rest -> Some (env :: updated_rest)
          | None -> Some (env :: rest))
  in
  match (s, helper s) with
  | _, Some updated_scope -> updated_scope
  | env :: rest, None -> StringMap.add x v env :: rest
  | _ -> failwith "Scope stack is empty"

(** Introduces [x |-> v] in the innermost scope, shadowing any binding of [x] in
    an outer scope (or overwriting it in the innermost one). Starts a new
    innermost scope if the stack is empty. Unlike {!update}, which rewrites an
    existing binding wherever it lives, this always binds in the head scope —
    the behaviour a block-scoped declaration needs. *)
let declare x v = function
  | env :: rest -> Ok (StringMap.add x v env :: rest)
  | [] -> Error (Fmt.str "Variable %s not declared" x)

(** Adds a new variable to the innermost scope. Fails if the scope stack is
    empty. *)
let push_scope s = StringMap.empty :: s

(** Removes the innermost scope from the scope stack. Returns None if the scope
    stack is empty. *)
let pop_scope s = match s with [] -> failwith "Scope stack is empty" | _ :: rest -> rest

(* Sequences a list of results into a result of a list, short-circuiting on the
   first error. *)
let rec sequence_results = function
  | [] -> Ok []
  | Error err :: _ -> Error err
  | Ok x :: rest -> (
      match sequence_results rest with
      | Ok xs -> Ok (x :: xs)
      | Error err -> Error err)
