open Symbolic.Data
open Symbolic.Runtime
open Soteria.Symex
open PolicyChecker.Data
open Utils.Data
open Utils.Loc

(** State monad signature:
    - [ok] is the type of the state that is threaded through the monad;
    - [err] is the type of the error that can be returned by computations, which
      will short-circuit the monadic computation. *)
module type S = sig
  type ok
  type err
end

(** Functor that produces a state monad given a signature [S] defining the types
    of the state and error. *)
module Make (S : S) : sig
  type ok = S.ok
  type err = S.err
  type ('a, 'fix) t = ok -> ('a * ok, err, 'fix) Symex.Result.t

  val return : 'a -> ('a, 'fix) t
  val bind : ('a, 'fix) t -> ('a -> ('b, 'fix) t) -> ('b, 'fix) t
  val map : ('a, 'fix) t -> ('a -> 'b) -> ('b, 'fix) t
  val run : ('a, 'fix) t -> ok -> ('a * ok, err, 'fix) Symex.Result.t
  val run_unit : (unit, 'fix) t -> ok -> (ok, err, 'fix) Symex.Result.t
  val get : (ok, 'fix) t
  val modify : (ok -> ok) -> (unit, 'fix) t
  val lift_symex_result : ('a, err, 'fix) Symex.Result.t -> ('a, 'fix) t
  val lift_symex : 'a Symex.t -> ('a, 'fix) t

  val fold_list :
    'a list -> init:'b -> f:('b -> 'a -> ('b, 'fix) t) -> ('b, 'fix) t

  val map_list : 'a list -> f:('a -> ('b, 'fix) t) -> ('b list, 'fix) t
  val ( let& ) : ('a, 'fix) t -> ('a -> ('b, 'fix) t) -> ('b, 'fix) t
  val ( let&* ) : 'a Symex.t -> ('a -> ('b, 'fix) t) -> ('b, 'fix) t

  val ( let&** ) :
    ('a, err, 'fix) Symex.Result.t -> ('a -> ('b, 'fix) t) -> ('b, 'fix) t

  val ( let&+ ) : 'a Symex.t -> ('a -> 'b) -> ('b, 'fix) t
  val ( let&++ ) : ('a, err, 'fix) Symex.Result.t -> ('a -> 'b) -> ('b, 'fix) t
end

(** State monad for encoding expression evaluation computations. The state
    carried through the monad is a function environment, which is possibly
    updated by function applications or nondets. *)
module ExpressionMonad : sig
  include module type of Make (struct
    type ok = function_env env
    type err = error_cause located
  end)
end

(** State monad for encoding statement evaluation computations. The state
    carried through the monad is a pair of an [ok_state], containing the current
    symbolic runtime structures to be threaded through the computation, and a
    list of [policy_checkers], needed to enforce policy whose violation can be
    detected eagerly. *)
module StatementMonad : sig
  include module type of Make (struct
    type ok = ok_state * policy_checker list
    type err = not_ok_state
  end)
end
