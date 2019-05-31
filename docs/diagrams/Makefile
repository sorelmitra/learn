VPATH=.

PLANTUML=/usr/local/bin/plantuml
RSVG=/usr/local/bin/rsvg-convert
CONVERT=/usr/local/bin/convert

PLANTUMLS:= \
	BotAggSolution.pu \

PLANTUML_SVGS:=$(PLANTUMLS:.pu=.svg)
PLANTUML_PNGS:=$(PLANTUMLS:.pu=.png)

all: $(PLANTUML_SVGS)
dist: all $(PLANTUML_PNGS)

%.svg: %.pu
	${PLANTUML} -tsvg $<

%.png: %.svg
	#${CONVERT} $< $@
	${RSVG} -h 3000 -b white $< >$@

clean: distclean
	rm -fv $(PLANTUML_SVGS)

distclean:
	rm -fv $(PLANTUML_PNGS)
