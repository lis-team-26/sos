let mode = Soteria.Symex.Approx.OX

(* Utils for lexer *)
let previous_token = ref ""
let current_token = ref ""

let lexer_with_history lexbuf =
  previous_token := !current_token;
  let tok = Lib.ContractLexer.read lexbuf in
  current_token := Lexing.lexeme lexbuf;
  tok


let () =
  if Array.length Sys.argv != 3 then Fmt.pr "Usage: %s <services_contract> <orchestrator_code>\n" Sys.argv.(0)
  else
    let contract_file = Sys.argv.(1) in
    let input_file =
      try open_in contract_file
      with Sys_error msg ->
        Printf.eprintf "Cannot open contract file: %s\n" msg;
        exit 1
    in
    
    let lexbuf = Lexing.from_channel input_file in

    try
      let ast = (Lib.ContractParser.prg lexer_with_history lexbuf) in
      Printf.printf "Parse successful\n";
      let fmt = Format.std_formatter in
      Lib.ContractLang_pp.pp_program fmt ast;
      Format.pp_print_flush fmt ();
      close_in input_file
    with
      | Lib.ContractParser.Error ->
          close_in input_file;
          let pos = lexbuf.lex_curr_p in
          let _token = Lexing.lexeme lexbuf in
          Printf.eprintf 
            "Parse error at line %d, column %d\n\
            previous token: '%s'\n\
            current token: '%s'\n"
            pos.pos_lnum
            (pos.pos_cnum - pos.pos_bol + 1)
            !previous_token
            !current_token;
          exit 1

      | exn -> close_in input_file; Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string exn);
  

    let src = Sys.argv.(2) in
    let prog = Lib.parse src in
    let final_states = Lib.symb_run prog ~mode in
    final_states
    |> List.mapi Lib.Utils.string_of_state
    |> String.concat "\n" |> print_endline
