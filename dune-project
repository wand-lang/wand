(lang dune 3.23)

(name wand)

(generate_opam_files true)

(source
 (github wand-lang/wand))

(authors "Mark Stahl <mark@technicallycorrect.company>")

(maintainers "Mark Stahl <mark@technicallycorrect.company>")

(license MIT)

(package
 (name wand)
 (synopsis "A typed ML-style scripting language for Human-AI collaboration")
 (description
  "wand is a typed, ML-style scripting language designed for Human-AI collaboration, with Hindley-Milner type inference, pattern matching, and Result-based error handling.")
 (depends
  (ocaml
   (= 5.5.1))
  (dune
   (= 3.24.2))
  (linenoise
   (= 1.5.1))
  (yojson
   (= 3.0.0))
  (toml
   (= 7.1.0))
  (yaml
   (= 3.2.0))
  (re
   (= 1.14.0))
  (digestif
   (= 1.3.1))
  (eqaf
   (= 0.10))
  (spawn
   (= v0.17.0))
  (alcotest
   (and
    (= 1.9.1)
    :with-test)))
 (tags
  ("scripting-language" "type-inference" "ml" "interpreter")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
