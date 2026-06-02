open Nfa

(** Convert a regex to an ε-free NFA using a slight modification of
    Glushkov's algorithm.

    (The modification: we label character sets rather than characters
     to prevent a state explosion.)
 *)
module C = Set.Make(Char)
type charset = C.t

(** A 'letter' is a character set paired with an identifier that
    uniquely identifies the character set within the regex *)
module Letter =
struct
  type t = C.t * state
  let compare (_,x) (_,y) = Stdlib.compare x y
end

(** Sets of single letters *)
module LetterSet =
struct
  module S = Set.Make(Letter)
  let (<+>) = S.union
end

(** Sets of letter pairs *)
module Letter2Set =
struct
  module Pair = struct
    type t = Letter.t * Letter.t
    let compare ((_,w),(_,x)) ((_,y),(_,z)) =
      Stdlib.compare (w,x) (y,z)
  end
  module S = Set.Make(Pair)
  let (<+>) = S.union
  let (>>=) m k = LetterSet.S.fold (fun x s -> k x <+> s) m S.empty
  let (<*>) : LetterSet.S.t -> LetterSet.S.t -> S.t =
    fun l r ->
      l >>= fun x ->
      r >>= fun y ->
      S.singleton (x, y)
end

type 'c regex =
    Empty : 'c regex (* L = { } *)
  | Eps   : 'c regex (* L = {ε} *)
  | Char  : 'c -> 'c regex
  | Alt   : 'c regex * 'c regex -> 'c regex
  | Seq   : 'c regex * 'c regex -> 'c regex
  | Star  : 'c regex -> 'c regex

(** Λ(r) is {ε} ∩ L(r); we represent it as a bool *)
let rec l = function
  | Empty  -> false
  | Eps    -> true
  | Char _ -> false
  | Alt (e, f) -> l e || l f
  | Seq (e, f) -> l e && l f
  | Star _     -> true

(** firsts: P(r) = {c | ∃s.cs ∈ L(r) } *)
let rec p = let open LetterSet in function
  | Empty
  | Eps    -> S.empty
  | Char c -> S.singleton c
  | Alt (e, f) -> p e <+> p f
  | Seq (e, f) -> p e <+> (if l e then p f else S.empty)
  | Star e     -> p e

(** lasts: D(r) = {c | ∃s.sc ∈ L(r) } *)
let rec d = let open LetterSet in function
  | Empty
  | Eps    -> S.empty
  | Char c -> S.singleton c
  | Alt (f, e) -> d f <+> d e
  | Seq (f, e) -> (if l e then d f else S.empty) <+> d e
  | Star e     -> d e

(** factors of length 2: F(r) = {c₁c₂ | ∃s₁s₂.s₁c₁c₂s₂ ∈ L(R)} *)
let rec f_ = let open Letter2Set in function
  | Empty
  | Eps
  | Char _ -> S.empty
  | Alt (e, f) -> f_ e <+> f_ f
  | Seq (e, f) -> f_ e <+> f_ f <+> (d e <*> p f)
  | Star e     -> f_ e <+> (d e <*> p e)

module StateMap = Map.Make(Int32)

module CharSetMap = Map.Make(C)

let add_transition2 c i tm =
  let ss = match CharSetMap.find c tm with
    | exception Not_found -> StateSet.empty
    | ss -> ss
  in CharSetMap.add c (StateSet.add i ss) tm

let add_transition i1 c2 i2 sm =
  let tm = match StateMap.find i1 sm with
    | exception Not_found -> CharSetMap.empty
    | ss -> ss
  in StateMap.add i1 (add_transition2 c2 i2 tm) sm

let transition_map_of_factor_set fs = 
  Letter2Set.S.fold
    (fun ((_,i1), (c2,i2)) sm -> add_transition i1 c2 i2 sm)
    fs
     StateMap.empty

let positions : LetterSet.S.t -> StateSet.t =
  fun s -> StateSet.of_list (List.map snd (LetterSet.S.elements s))

let transition_map_of_letter_set : LetterSet.S.t -> StateSet.t CharSetMap.t =
  fun s ->
    LetterSet.S.fold
      (fun (c,i) tm ->
         let entry = match CharSetMap.find c tm with
           | exception Not_found -> StateSet.singleton i
           | s -> StateSet.add i s
         in CharSetMap.add c entry tm)
      s
      CharSetMap.empty

type t = C.t regex

let fresh_state =
  let counter = ref 0l in
  let incr32 r = r := Int32.succ !r in
  fun () -> let c = !counter in incr32 counter; c

let start_state = fresh_state ()

let rec annotate : 'a. 'a regex -> ('a * int32) regex = function 
  | Empty  -> Empty
  | Eps    -> Eps
  | Char c -> let p = (c, fresh_state ()) in Char p
  | Alt (e, f) -> Alt (annotate e, annotate f)
  | Seq (e, f) -> Seq (annotate e, annotate f)
  | Star e     -> Star (annotate e)

let flatten_transitions : StateSet.t CharSetMap.t -> StateSet.t CharMap.t =
  fun cm ->
    CharSetMap.fold
      (fun cs ss cm ->
         C.fold
           (fun c cm ->
              let entry = match CharMap.find c cm with
                | exception Not_found -> StateSet.empty
                | ss -> ss in
              CharMap.add c (StateSet.union ss entry) cm)
             cs
             cm)
      cm
      CharMap.empty

let compile r =
  (* Give every character set in 'r' a unique identifier *)
  let r = annotate r in

  (* The final states are the set of 'last' characters in r,
     (+ the start state if r accepts the empty string) *)
  let finals =
    if l r then StateSet.add start_state (positions (d r))
    else positions (d r)
  in

  (* Transitions arise from factor (pairs of character sets with a
     transition between them) ... *)
  let transitions = transition_map_of_factor_set (f_ r) in

  (* ... and from the start state to the initial character sets. *)
  let initial_transitions = transition_map_of_letter_set (p r) in
  let joint_transitions = StateMap.add start_state initial_transitions transitions in

  (* The 'next' function is built from the transition sets. *)
  let next s =
    try flatten_transitions (StateMap.find s joint_transitions)
    with Not_found -> CharMap.empty
  in
  let nfa = 
  { start = StateSet.singleton start_state; finals; next }
  in
  Dfa.minimize @@ Dfa.determinize nfa

(** Various basic and derived regex combinators *)
let seq l r =
  match l, r with
  | Eps, s | s, Eps -> s
  | l, r -> Seq (l, r)
let alt l r =
  match l, r with
    Char c1, Char c2 -> Char (C.union c1 c2)
  | l, r -> Alt (l, r)
let star r = Star r
let plus t = seq t (star t)
let eps = Eps
let chr c = Char (C.singleton c)
let opt t = alt t eps
let empty = Empty
let range_ maybe_domain l h =
  let rec loop i h acc =
    if i = h then         C.add (Char.chr i) acc
    else loop (succ i) h (C.add (Char.chr i) acc)
  in 
  let in_range = loop (Char.code l) (Char.code h) C.empty in
  match maybe_domain with 
    |None -> in_range
    |Some d -> C.inter d in_range

let range maybe_domain l h  = Char (range_ maybe_domain l h)
let any_ maybe_domain = range_ maybe_domain (Char.chr 0) (Char.chr 255)
let any maybe_domain = Char (any_  maybe_domain)
type parse_error = Generic | Not_in_domain of char | Bad_range of char * char
module Parse =
struct
  exception Error of parse_error
  let check_domain_exn c maybe_d = 
    match maybe_d with
    | None -> ()
    | Some d -> if C.mem c d then () else raise @@ Error (Not_in_domain c)
  module Bracket =
  struct
    (** Follows the POSIX spec
          9.3.5 RE Bracket Expression
           http://pubs.opengroup.org/onlinepubs/009696899/basedefs/xbd_chap09.html#tag_09_03_05
        but there is currently no support for character classes.

        Bracket expressions are delimited by [ and ], with an optional "complement"
        operator ^ immediately after the [.  Special characters:
           ^ (immediately after the opening [)
           ] (except immediately after the opening [ or [^)
           - (except at the beginning or end)                                   *)

    type element = Char of char | Range of char * char
    type t = { negated: bool; elements: element list }

    let interpret maybe_domain { negated; elements } =
    let add_char c acc =
      check_domain_exn c maybe_domain;
      C.add c acc
      in
    let add_range c1 c2 acc =
      check_domain_exn c1 maybe_domain;
      check_domain_exn c2 maybe_domain;
      if c1 > c2 then raise @@ Error (Bad_range (c1,c2))
        else C.union (range_ maybe_domain c1 c2) acc      
  in
  let fold_fun acc elem =
    match elem with
    | Char c -> add_char c acc
    | Range (c1,c2) -> add_range c1 c2 acc
  in
      let s =
        List.fold_left fold_fun
          C.empty elements in
      if negated then C.diff (any_ maybe_domain) s
      else s

    let parse_element  = function
      | [] -> raise @@ Error Generic
      | ']' :: s -> (None, s)
      | c :: ('-' :: ']' :: _ as s) -> (Some (Char c), s)
      | c1 :: '-' :: c2 :: s -> (Some (Range (c1, c2)), s)
      | c :: s -> (Some (Char c), s)

    let parse_initial = function
      | [] ->  raise @@ Error Generic
      | c :: ('-' :: ']' :: _ as s) -> (Some (Char c), s)
      | c1 :: '-' :: c2 :: s -> (Some (Range (c1, c2)), s)
      | c :: s -> (Some (Char c), s)

    let parse_elements s = 
      let rec loop elements s =
        match parse_element s with
        | None, s -> (List.rev elements, s)
        | Some e, s -> loop (e :: elements) s in
      match parse_initial s with
      | None, s -> ([], s)
      | Some e, s -> loop [e] s
  end

  type t =
    | Opt : t -> t
    | Chr : char -> t 
    | Alt : t * t -> t
    | Seq : t * t -> t
    | Star : t -> t
    | Plus : t -> t
    | Bracketed : Bracket.t -> t
    | Any : t
    | Eps : t

  let rec interpret maybe_domain reg =
    let interpret = interpret maybe_domain in
    match reg with
    | Opt t -> opt (interpret t)
    | Chr c -> 
      check_domain_exn c maybe_domain;  
      chr c
    | Alt (l, r) -> alt (interpret  l) (interpret  r)
    | Seq (l, r) -> seq (interpret l) (interpret r)
    | Star t -> star (interpret t)
    | Plus t -> plus (interpret t)
    | Bracketed elements -> Char (Bracket.interpret maybe_domain elements)
    | Any -> (any maybe_domain)
    | Eps -> eps

   (* We've seen [.  Special characters:
        ^   (beginning of negation)  *)
  let re_parse_bracketed : char list -> (t * char list) = function
    | '^' :: s -> let elements, rest = Bracket.parse_elements s in
                  (Bracketed { negated = true; elements }, rest)
    | _ :: _ as s -> let elements, rest = Bracket.parse_elements s in
                (Bracketed { negated = false; elements }, rest)
    | [] ->  raise @@ Error Generic

  (** ratom ::= .
                <character>
                [ bracket ]
                ( ralt )           *)
  let rec re_parse_atom : char list -> (t * char list) option = function
    | '('::rest ->
       begin match re_parse_alt rest with
       | (r, ')' :: rest) -> Some (r, rest)
       | _ ->  raise @@ Error Generic

       end
    | '['::rest -> Some (re_parse_bracketed rest)
    | [] | ((')'|'|'|'*'|'?'|'+') :: _) -> None
    | '.' :: rest -> Some (Any, rest)
    | h :: rest -> Some (Chr h, rest)

  (** rsuffixed ::= ratom
                    atom *  
                    atom +
                    atom ?         *)
 and re_parse_suffixed : char list -> (t * char list) option =
    fun s -> match re_parse_atom s with
    | None -> None
    | Some (r, '*' :: rest) -> Some (Star r, rest)
    | Some (r, '+' :: rest) -> Some (Plus r, rest)
    | Some (r, '?' :: rest) -> Some (Opt r, rest)
    | Some (r, rest) -> Some (r, rest)

  (** rseq ::= <empty>
               rsuffixed rseq      *)
  and re_parse_seq : char list -> t * char list = fun (s: char list) ->
    match re_parse_suffixed s with
    | None -> (Eps, s)
    | Some (r, rest) -> let r', s' = re_parse_seq rest in (Seq (r, r'), s')

  (** ralt ::= rseq
               rseq | ralt         *)
  and re_parse_alt (s: char list) =
    match re_parse_seq s with
    | (r, '|' :: rest) -> let r', s' = re_parse_alt rest in (Alt (r, r'), s')
    | (r, rest) -> (r, rest)

  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []

  let parse s =
    match re_parse_alt (explode s) with
    | (r, []) -> r
    | (_, _::_) -> raise @@ Error Generic

end

exception Parse_error of string * parse_error
let parse ?domain s = 
  try Parse.(interpret domain (parse s))
  with
  | Parse.Error(e) -> raise (Parse_error (s,e))

let reg2dfa ?domain s = compile @@ parse ?domain s
