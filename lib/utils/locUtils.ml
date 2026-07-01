open DataUtils

type source_pos = { line : int; col : int; offset : int }
(** A position in the source code. *)

(** A location in the source code; it can be either a [Loc], representing a
    range of characters in the source code, or a [EOFLoc], representing the end
    of the file. *)
type loc =
  | Loc of { file : string; start_pos : source_pos; end_pos : source_pos }
  | EOFLoc

type 'a located = { it : 'a; at : loc }
(** An item of type ['a] with an associated source code location. *)

(** Creates a located item *)
let located ~loc item = { it = item; at = loc }

(** Helper function to drop the location from a located item. *)
let drop_loc { it; _ } = it

(** Helper function to map over the item in a located value. *)
let map_loc f { it; at } = { it = f it; at }

(** Returns an [Error] with a located message, computed using the format [fmt].
*)
let located_error ~loc fmt = Fmt.kstr (located ~loc >> Result.error) fmt

(** Creates a located item given its source code positions; used while parsing
    with lexer positions. *)
let located_with_positions item ~start_pos ~end_pos =
  let open Lexing in
  located item
    ~loc:
      (Loc
         {
           file = start_pos.pos_fname;
           start_pos =
             {
               line = start_pos.pos_lnum;
               col = start_pos.pos_cnum - start_pos.pos_bol + 1;
               offset = start_pos.pos_cnum;
             };
           end_pos =
             {
               line = end_pos.pos_lnum;
               col = end_pos.pos_cnum - end_pos.pos_bol + 1;
               offset = end_pos.pos_cnum;
             };
         })
