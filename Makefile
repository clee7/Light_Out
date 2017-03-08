default: compile

NAME = lab7

all: compile test

compile: clean
	ocamlfind ocamlc -c ${NAME}.mli ${NAME}.ml

test:
	ocamlfind ocamlc -o tests_${NAME} \
	  -package oUnit -linkpkg \
	  ${NAME}.cmo tests_${NAME}.ml
	./tests_${NAME}

clean:
	rm -f *.cm* *.log *.cache tests_${NAME}
