(lang dune 3.22)

(name lis-project)

(generate_opam_files true)

(source
 (github lis-team-26/lis-project))

(authors "Students of the LIS course")

(maintainers "Students of the LIS course")

(license GPL-3.0-or-later)

(using menhir 3.0)

(package
 (name lis-project)
 (synopsis "Symbolic framework for the analysis of service orchestration")
 (description "Symbolic framework for tha analysis of service orchestration")
 (depends
  ocaml
  dune
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
