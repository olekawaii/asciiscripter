MAIN      = main.hs
TARGET    = main

HS_FILES  = $(wildcard src/*.hs)

all: $(TARGET)

$(TARGET): $(HS_FILES)
	@mkdir -p build
	ghc $(GHCFLAGS) -o $(TARGET) -Wall -isrc -outputdir build src/$(MAIN)

clean:
	rm -f $(TARGET) build/*.o build/*.hi

run: $(TARGET)
	./$(TARGET)

install : $(TARGET)
	doas cp ./scripts/uwu ./$(TARGET) /usr/local/bin
	  
