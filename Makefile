EXECUTABLE = ascr-to-shell
EXECUTABLE2 = ascr

all: $(EXECUTABLE) $(EXECUTABLE2)

$(EXECUTABLE): $(wildcard src-ascr-to-shell/*.hs)
	mkdir -p build
	ghc -o $(EXECUTABLE) -isrc-ascr-to-shell -outputdir build src-ascr-to-shell/main.hs

$(EXECUTABLE2): $(wildcard ascr/*.rs)
	cd src-ascr && cargo build --release && mv target/release/ascr ../ascr

clean:
	rm -rf build $(EXECUTABLE)

install : $(EXECUTABLE) $(EXECUTABLE2)
	mkdir -p ~/.local/bin
	cp scripts/uwu $(EXECUTABLE) ascr ~/.local/bin

