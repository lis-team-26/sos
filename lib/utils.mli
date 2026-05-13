open SymbolicInterpreter

val string_of_state :
  int ->
  (ok_state, err_state, 'a) Soteria.Symex.Compo_res.t
  * Symex.Value.sbool Symex.Value.t list ->
  string
