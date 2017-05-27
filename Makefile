OC := ocamlbuild -classic-display -no-links -use-ocamlfind

backtrace: parser.ml lexer.ml
	ocamlbuild -tag 'debug' -r -use-ocamlfind -package extlib parser.native
	OCAMLRUNPARAM=b ./parser.native

test_suite:
	ocamlbuild -tag 'debug' -r -use-ocamlfind -package oUnit,extlib test.native
	./test.native

build:
	$(OC) -tag thread -pkg threads,utop,extlib myutop.top
	mv _build/myutop.top .
	chmod +x myutop.top

clean:
	$(OC) -clean
