GHC       = ghc -Wall
MAIN      = main.hs
TARGET    = main

HS_FILES  = $(wildcard src/*.hs)

all: $(TARGET)

$(TARGET): $(HS_FILES)
	@mkdir -p build
	# cd src && $(GHC) $(GHCFLAGS) -o $(TARGET) -outputdir ../build $(MAIN) && mv main ..
	$(GHC) $(GHCFLAGS) -o $(TARGET) -isrc -outputdir build src/$(MAIN)

clean:
	rm -f $(TARGET) build/*.o build/*.hi

run: $(TARGET)
	./$(TARGET)

install : $(TARGET)
	doas cp ./scripts/uwu ./$(TARGET) /usr/local/bin
	  
