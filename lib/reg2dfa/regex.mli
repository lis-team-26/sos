type t
(** The type of regular expressions. *)

type charset = Set.Make(Char).t
(** Sets of characters. *)

val parse : ?domain:charset -> string -> t
(** Parse a regular expression using the following grammar:

    [r ::=]
    - [(r)] (parenthesized regex)
    - [.] (match any character)
    - [rr] (sequencing)
    - [r|r] (alternation)
    - [r?] (zero or one)
    - [r*] (zero or more)
    - [r+] (one or more)
    - [c] (literal character)
    - [[b]] (POSIX bracket expression)

    Raises [Parse_error] on parse error. *)

val compile : t -> Dfa.dfa
(** [compile r] translates [r] the minimized DFA that succeeds on exactly those
    strings matched by [r] *)

val reg2dfa : ?domain:charset -> string -> Dfa.dfa

type parse_error = Generic | Not_in_domain of char | Bad_range of char * char

exception Parse_error of string * parse_error
(** Raised when [parse] is given an invalid regex. *)
