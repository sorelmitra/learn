VPATH=..

# TypeScript to PlantUML
TPLANT=/usr/local/bin/tplant

PLANTUML=/usr/local/bin/plantuml
CONVERT=/usr/local/bin/convert

PLANTUMLS:= \
	generated-class-hierarchy.puml \
	test.puml \

PLANTUML_SVGS:=$(PLANTUMLS:.puml=.svg)
PLANTUML_PNGS:=$(PLANTUMLS:.puml=.png)

all: $(PLANTUML_SVGS)
dist: $(PLANTUML_PNGS)

%.svg: %.puml
	${PLANTUML} -tsvg $< $@

%.png: %.svg
	${CONVERT} $< $@

class-hierarchy:
	${TPLANT} --associations \
		--input src/**/*.ts \
		--output generated-class-hierarchy.puml

clean: distclean
	rm -fv $(PLANTUML_SVGS)

distclean:
	rm -fv $(PLANTUML_PNGS)
