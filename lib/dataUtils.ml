module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

type 'a env = 'a StringMap.t
type 'a scope = 'a env list

let rec lookup x s =
  match s with
  | [] -> None
  | env :: rest -> (
      match StringMap.find_opt x env with
      | Some v -> Some v
      | None -> lookup x rest)
