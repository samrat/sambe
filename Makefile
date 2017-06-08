OC := ocamlbuild -classic-display -no-links -use-ocamlfind

sambe:
	ocamlbuild -tag 'debug' -r -use-ocamlfind -package extlib -I src/ sambe.native

sambe.byte:
	ocamlbuild -tag 'debug' -r -use-ocamlfind -package extlib -I src/ sambe.byte

test_suite:
	ocamlbuild -tag 'debug' -r -use-ocamlfind -package oUnit,extlib -I tests/ -I src/ test.native
	./test.native

test: test.s
	nasm -f elf64 test.s
	gcc -c test_main.c
	gcc test.o test_main.o
	rm test_main.o
	rm test.o

clean:
	$(OC) -clean
