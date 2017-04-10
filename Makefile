backtrace: parser.ml lexer.ml
	ocamlbuild -tag 'debug' -r -use-ocamlfind -package extlib parser.native
	OCAMLRUNPARAM=b ./parser.native

test_parser: util.ml instr.ml lexer.ml parser.ml test_parser.ml test_lexer.ml test.ml
	ocamlfind ocamlc -o test_parser -package oUnit,extlib -linkpkg -g $^
	./test_parser
