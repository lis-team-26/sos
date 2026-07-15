(lang dune 3.22)

(name sos)

(generate_opam_files true)

(source
 (github lis-team-26/lis-project))

(authors
 "Francesco Borri, Lorenzo Ceccotti, Tommaso Crocetti, Federico Fornaciari, Andrea Malatesti, Eduardo Meli, Angelo Passarelli, Anna Ricci, Daniele Sampietro, Lorenzo Maria Siverino, Natalija Tosic")

(maintainers
 "Francesco Borri, Lorenzo Ceccotti, Tommaso Crocetti, Federico Fornaciari, Andrea Malatesti, Eduardo Meli, Angelo Passarelli, Anna Ricci, Daniele Sampietro, Lorenzo Maria Siverino, Natalija Tosic")

(license LGPL-3.0-or-later)

(using menhir 3.0)

(package
 (name sos)
 (synopsis "Symbolic Orchestation w/ Soteria")
 (description
  "Symbolic framework for the analysis of service orchestration, built on top of the Soteria symbolic execution engine")
 (depends
  ocaml
  dune
  fmt
  menhir
  ppx_blob
  ppx_subliner
  soteria
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.29.0)))
  (ocaml-lsp-server :with-dev-setup)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
