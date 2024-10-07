(lang dune 3.16)

(name learn_you_ocaml)

(generate_opam_files true)

(source
 (github developerSid/learn-you-ocaml))

(authors "Gary Myers")

(maintainers "Gary Myers")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name learn_you_ocaml)
 (synopsis "My attempt at learning OCaml")
 (description "Learning OCaml for the second time")
 (depends ocaml=5.2.0 dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
