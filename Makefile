OC := ocamlbuild -classic-display -no-links -use-ocamlfind

sambe:
	ocamlbuild -tag 'debug' -r -use-ocamlfind -package extlib sambe.native

test_suite:
	ocamlbuild -tag 'debug' -r -use-ocamlfind -package oUnit,extlib -Is tests/ test.native
	./test.native

build: myutop.mltop
	rm -f myutop.top
	ocamlbuild -clean
	$(OC) -tag thread -pkg threads,utop,extlib myutop.top

	mv _build/myutop.top .
	chmod +x myutop.top

test: test.s
	nasm -f elf64 test.s
	gcc -c test_main.c
	gcc test.o test_main.o
	rm test_main.o
	rm test.o

clean:
	$(OC) -clean
