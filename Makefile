EXECUTABLE = ascr

all: $(EXECUTABLE)

$(EXECUTABLE): $(wildcard src/*.hs)
	mkdir -p build
	ghc -o $(EXECUTABLE) -isrc -outputdir build src/main.hs

clean:
	rm -rf build $(EXECUTABLE)

run: $(EXECUTABLE)
	$(EXECUTABLE)

install : $(EXECUTABLE)
	doas cp scripts/uwu $(EXECUTABLE) /usr/local/bin
	  
