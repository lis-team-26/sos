open Symbolic.Data
open Symbolic.Runtime
open Soteria.Symex
open PolicyChecker
open Utils.Data

module type S = sig
  type ok
  type err
end

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

module FunctionalMonad : sig
  include module type of Make (struct
    type ok = function_env env
    type err = violation_id
  end)
end

module OkStateMonad : sig
  include module type of Make (struct
    type ok = ok_state * policyChecker list
    type err = err_state
  end)
end
