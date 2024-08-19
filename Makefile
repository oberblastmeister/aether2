dev:
	opam switch create -y . 5.2.0 --deps-only --with-test --with-doc
	opam install -y dune merlin ocamlformat utop ocaml-lsp-server

deps:
	opam install -y . --deps-only

watch:
	dune test --watch --terminal-persistence=clear-on-rebuild
