(lang dune 2.0)

(name microc-lang)

(generate_opam_files true)

(authors "LCI-2022")

(maintainers "LCI-2022")

(license ISC)

(using menhir 2.0)

(package
 (name microc-lang)
 (synopsis "microc is a simple subset of the C language.")
 (description "microc is a simple subset of the C language.")
 (depends
  (ocaml (and (>= 4.12) (< 5.0)))
   dune
   menhir
   ppx_deriving
   (llvm (and (>= 12.0.0)(< 15.0.0)))))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
