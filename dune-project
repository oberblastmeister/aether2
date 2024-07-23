(lang dune 3.13)

(name aether2)

(generate_opam_files true)

(using menhir 3.0)

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
  (core
   (= v0.17.1))
  (core_bench
   (= v0.17.1))
  (ppx_deriving
   (= 5.2.1))
  (ppx_jane
   (= v0.17.1))
  (alcotest
   (= 1.7.0))
  (algaeff
   (= 2.0.0))
  (higher_kinded
   (= v0.16.0))
  (eio
   (= 0.15))
  (eio_main
   (= 0.15))
  (zarith
   (= 1.13))
  int_repr)
 (tags
  (topics "to describe" your project)))
