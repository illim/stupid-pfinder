all:
	ocamlbuild Main.p.native

clean:
	ocamlbuild -$@

run: all
	./Main.p.native

runp: all
	./Main.p.native
	gprof Main.p.native | less

.PHONY : all clean run runp