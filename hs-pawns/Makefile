dist/setup-config: pawns.cabal
	cabal configure --enable-executable-profiling

dist: dist/setup-config *.hs
	cabal build

clean:
	cabal $@

install:
	cabal install --prefix=$HOME --user

run: dist
	./dist/build/pawns/pawns

runp: dist
	./dist/build/pawns/pawns +RTS -s -p -hc -RTS
	cat pawns.prof

ps: runp
	hp2ps -e8in -c pawns.hp

.PHONY: clean run runp ps