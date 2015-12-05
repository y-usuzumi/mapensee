all: HelloWorld.so

HelloWorld.so: HelloWorld.hs
	ghc -O2 --make -no-hs-main -dynamic -shared -o $@ $^
