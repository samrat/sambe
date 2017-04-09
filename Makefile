backtrace: parser.ml lexer.ml
	ocamlbuild -tag 'debug' -r -use-ocamlfind -package extlib parser.native
	OCAMLRUNPARAM=b ./parser.native
