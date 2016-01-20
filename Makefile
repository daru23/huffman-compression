BINARIES = Main
SOURCES  = Main.hs
MODULES  = Huffman.hs CompresorDeArchivos.hs

all:	$(BINARIES)

Main: $(SOURCES) $(MODULES)
	  ghc -o Main --make Main.hs 

clean:
	rm Main.exe

