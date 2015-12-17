all: HelloWorld.exe

HelloWorld.exe: HelloWorld.so HelloWorld.cs
	mcs -unsafe HelloWorld.cs

HelloWorld.so: HelloWorld.hs
	ghc -O2 --make -dynamic -shared -o $@ $^

clean:
	rm HelloWorld.hi HelloWorld.o HelloWorld.so HelloWorld_stub.h HelloWorld.exe
