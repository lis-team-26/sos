(** Extracts the value from a result, or fails with a formatted error message if
    it's an error. *)
let get_or_fail ~pp = function
  | Ok v -> v
  | Error msg ->
      Fmt.epr "%a@." pp msg;
      exit 1

(* Sequences a list of results into a result of a list, short-circuiting on the
   first error. *)
let rec all_ok = function
  | [] -> Ok []
  | Error err :: _ -> Error err
  | Ok x :: rest -> (
      match all_ok rest with Ok xs -> Ok (x :: xs) | Error err -> Error err)

(** Sequences a list of unit results into a unit result, short-circuiting on the
    first error. *)
let rec all_unit_ok = function
  | [] -> Ok ()
  | Error err :: _ -> Error err
  | Ok () :: rest -> all_unit_ok rest
