let mode = Soteria.Symex.Approx.OX

let () =
  if Array.length Sys.argv != 2 then Fmt.pr "Usage: %s <src>\n" Sys.argv.(0)
  else
    let src = Sys.argv.(1) in
    let prog = Lib.parse src in
    let final_states = Lib.symb_run prog ~mode in
    final_states
    |> List.mapi Lib.Utils.string_of_state
    |> String.concat "\n" |> print_endline
