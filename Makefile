OC := ocamlbuild -classic-display -no-links -use-ocamlfind

backtrace: parser.ml lexer.ml
	ocamlbuild -tag 'debug' -r -use-ocamlfind -package extlib parser.native
	OCAMLRUNPARAM=b ./parser.native

test_suite: util.ml instr.ml lexer.ml parser.ml cfg.ml test_parser.ml test_lexer.ml test.ml
	ocamlfind ocamlc -o test_suite -package oUnit,extlib -linkpkg -g $^
	./test_suite

build:
	$(OC) -tag thread -pkg threads,utop,extlib myutop.top

clean:
	$(OC) -clean
