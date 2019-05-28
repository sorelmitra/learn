VPATH=..

PLANTUML=/usr/local/bin/plantuml

PLANTUMLS:= \
	test.pu \

PLANTUML_SVGS:=$(PLANTUMLS:.pu=.svg)

all: $(PLANTUML_SVGS)

%.svg: %.pu
	${PLANTUML} -tsvg $< $@

clean:
	rm -fv $(PLANTUML_SVGS)
