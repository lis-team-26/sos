open Format
open LocUtils

let pp_loc fmt = function
  | Loc loc ->
      fprintf fmt "file '%s', lines %d-%d, characters %d-%d" loc.file
        loc.start_pos.line loc.end_pos.line loc.start_pos.col loc.end_pos.col
  | EOFLoc -> fprintf fmt "end of file"
