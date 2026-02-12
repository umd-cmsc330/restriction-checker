(lang dune 3.15)

(generate_opam_files true)

(name synopsis)

(package
  (name synopsis)
  (authors "umd-cmsc330" "ptrichr")
  (maintainers "umd-cmsc330")
  (synopsis "Examines the parsetree")
  (description "Synopsis gathers information about a program's parsetree. Namely, module usage, definitions, and function calls.")
  (license MIT)
  (source
    (github umd-cmsc330/restriction-checker))
  (depends 
    (ocaml (>= 5.2.0)) 
    dune
    (alcotest (>= 1.9.1)))
  (allow_empty))

; See the complete stanza docs at https://dune.readthedocs.io/en/latest/reference/dune-project/index.html
