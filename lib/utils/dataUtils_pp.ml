open DataUtils

let rec pp_list ?(sep = Fmt.cut) pp fmt = function
  | [] -> Fmt.pf fmt "<empty>"
  | [ x ] -> pp fmt x
  | x :: xs -> Fmt.pf fmt "%a%a%a" pp x sep () (pp_list ~sep pp) xs

let rec pp_list_inline pp fmt = pp_list ~sep:(Fmt.const Fmt.string ", ") pp fmt

let rec pp_entry pp_key pp_value fmt (key, value) =
  Fmt.pf fmt "%a -> %a" pp_key key pp_value value

and pp_env pp_key pp_value fmt env =
  pp_list (pp_entry pp_key pp_value) fmt (StringMap.bindings env)

let pp_env_inline pp_key pp_value fmt env =
  pp_list_inline (pp_entry pp_key pp_value) fmt (StringMap.bindings env)

let pp_section ~name ?(with_cut = false) pp =
  let pp fmt value = Fmt.pf fmt "%s {@,@[<v 2>  %a@]@,}" name pp value in
  if with_cut then Fmt.(pp ++ cut) else pp

let pp_field ~name ?(with_cut = false) pp =
  let pp fmt value = Fmt.pf fmt "%s:@,@[<v 2>  %a@]" name pp value in
  if with_cut then Fmt.(pp ++ cut) else pp
