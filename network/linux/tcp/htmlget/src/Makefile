all: htmlget

SRC=htmlget.c
BINDIR=../bin

dirs: $(BINDIR)
	mkdir -p $(BINDIR)

htmlget: $(SRC) dirs
	$(CC) -o $(BINDIR)/$@ $(SRC)

clean:
	rm -rfv $(BINDIR)
