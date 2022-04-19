VPATH=..

# TypeScript to PlantUML
TPLANT=/usr/local/bin/tplant

PLANTUML=/usr/local/bin/plantuml
# This is the ImageMagick converter
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
	${CONVERT} -size 3000 $< $@

class-hierarchy:
	${TPLANT} --associations \
		--input src/**/*.ts \
		--output generated-class-hierarchy.puml

# It'll show all the available fonts to PlantUML
show-fonts:
	${PLANTUML} -tsvg fonts.puml

clean: distclean
	rm -fv $(PLANTUML_SVGS)

distclean:
	rm -fv $(PLANTUML_PNGS)
