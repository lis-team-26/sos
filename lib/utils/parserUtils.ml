module type Parser = sig
  type ast
  type token

  exception LexerError of string
  exception ParserError

  val pp : Format.formatter -> ast -> unit
  val lexer : Lexing.lexbuf -> token
  val parser : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ast
end

module MakeParser (P : Parser) = struct
  let previous_token = ref ""
  let current_token = ref ""

  let lexer_with_history lexbuf =
    previous_token := !current_token;
    let tok = P.lexer lexbuf in
    current_token := Lexing.lexeme lexbuf;
    tok

  let parse src =
    let input_file =
      try open_in src
      with Sys_error msg ->
        Printf.eprintf "Could not open file '%s': %s\n" src msg;
        exit 1
    in
    let lexbuf = Lexing.from_channel input_file in
    try
      let ast = P.parser lexer_with_history lexbuf in
      close_in input_file;
      ast
    with
    | P.LexerError msg ->
        close_in input_file;
        Printf.eprintf "Lexer error in '%s': %s\n" src msg;
        exit 1
    | P.ParserError ->
        close_in input_file;
        let pos = lexbuf.lex_curr_p in
        let _token = Lexing.lexeme lexbuf in
        Printf.eprintf
          "Parse error in '%s', line %d, column %d\n\
           Previous token: '%s'\n\
           Current token: '%s'\n"
          src pos.pos_lnum
          (pos.pos_cnum - pos.pos_bol + 1)
          !previous_token !current_token;
        exit 1
    | exn ->
        close_in input_file;
        Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string exn);
        exit 1
end
