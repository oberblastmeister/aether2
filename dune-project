(lang dune 3.10)

(name aether2)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name aether2)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   ocaml
   dune
   (core (= v0.16.2))
   (core_bench (= v0.16.0))
   (ppx_deriving (= 5.2.1))
   (ppx_jane (= v0.16.0))
   (alcotest (= 1.7.0))
   (algaeff (= 2.0.0))
   (iter (= 1.8))
 )
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
