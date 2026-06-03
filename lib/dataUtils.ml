open Format
module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

type ident = string

type var_type = TInt | TBool

let rec pp_var_type fmt = function
  | TInt -> fprintf fmt "int"
  | TBool -> fprintf fmt "bool"


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
  | _, Some updated_scope -> Some updated_scope
  | env :: rest, None -> Some (StringMap.add x v env :: rest)
  | [], None -> None

(** Adds a new variable to the innermost scope. Fails if the scope stack is
    empty. *)
let push_scope s = StringMap.empty :: s

(** Removes the innermost scope from the scope stack. Returns None if the scope
    stack is empty. *)
let pop_scope s = match s with [] -> None | _ :: rest -> Some rest
