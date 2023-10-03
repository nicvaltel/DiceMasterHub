out/back-end-exe: package.yaml $(shell find app -type f) $(shell find src -type f)
	hpack; \
	cabal build; \
	cp ./dist-newstyle/build/x86_64-linux/ghc-9.2.8/back-end-0.1.0.0/x/back-end-exe/build/back-end-exe/back-end-exe ./out/
