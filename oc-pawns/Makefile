all:
	ocamlbuild Main.native

clean:
	ocamlbuild -$@

run: all
	./_build/Main.native

.PHONY : all clean run