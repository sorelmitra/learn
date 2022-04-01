VPATH=..

PLANTUML=/usr/local/bin/plantuml
CONVERT=/usr/local/bin/convert

PLANTUMLS:= \
	mock-sequence.puml \
	mock-topology.puml \

PLANTUML_SVGS:=$(PLANTUMLS:.puml=.svg)
PLANTUML_PNGS:=$(PLANTUMLS:.puml=.png)

all: $(PLANTUML_SVGS)
dist: $(PLANTUML_PNGS)

%.svg: %.puml
	${PLANTUML} -tsvg $< $@

%.png: %.svg
	${CONVERT} -size 3000 $< $@

clean: distclean
	rm -fv $(PLANTUML_SVGS)

distclean:
	rm -fv $(PLANTUML_PNGS)
