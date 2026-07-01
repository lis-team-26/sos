(** Utility module signature to provide common parsing functionality. *)
module type Parser = sig
  type ast
  type token

  exception LexerError of string
  exception ParserError

  val pp : ast Fmt.t
  val lexer : Lexing.lexbuf -> token
  val parser : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> ast
end

(** Builds a parser for the given parser interface. In particular, it exposes a
    [parse] function to parse source files handling common errors. *)
module Make (P : Parser) = struct
  open Result.Syntax

  let parse src =
    let* input_file =
      try Ok (open_in src)
      with Sys_error msg -> Fmt.error "Could not open file '%s': %s" src msg
    in
    let lexbuf = Lexing.from_channel input_file in
    Lexing.set_filename lexbuf src;
    try
      let ast = P.parser P.lexer lexbuf in
      close_in input_file;
      Ok ast
    with
    | P.LexerError msg ->
        close_in input_file;
        let pos = lexbuf.lex_curr_p in
        Fmt.error "Syntax error in file '%s', line %d, column %d: %s" src
          pos.pos_lnum
          (pos.pos_cnum - pos.pos_bol + 1)
          msg
    | P.ParserError ->
        close_in input_file;
        let pos = lexbuf.lex_curr_p in
        Fmt.error "Parse error in file '%s', line %d, column %d" src
          pos.pos_lnum
          (pos.pos_cnum - pos.pos_bol + 1)
    | exn ->
        close_in input_file;
        Fmt.error "Unexpected error: %s" (Printexc.to_string exn)
end
