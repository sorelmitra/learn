VPATH=..

DOT=/opt/local/bin/dot
UMLGRAPH=/usr/local/bin/umlgraph
PIC2PLOT=/opt/local/bin/pic2plot

DOTS:= \
	uml.dot \
	MarUseCases.dot \
	subgraphs.dot \

JAVAS:= \
	Shapes.java \
	Colors.java \

PICS= \
	sample_uml_seq.pic \

DOT_SVGS:=$(DOTS:.dot=.svg)
UMLGRAPH_CLASS_SVGS:=$(JAVAS:.java=.svg)
UMLGRAPH_SEQ_SVGS:=$(PICS:.pic=.svg)

all: $(DOT_SVGS) $(UMLGRAPH_CLASS_SVGS) $(UMLGRAPH_SEQ_SVGS)

%.svg: %.dot
	${DOT} -Tsvg $< >$@

%.svg: %.java
	JAVA_HOME=/Library/Java/JavaVirtualMachines/jdk1.8.0_60.jdk/Contents/Home ${UMLGRAPH} $* svg

%.svg: %.pic
	${PIC2PLOT} -T svg $< >$@

clean:
	rm -fv $(DOT_SVGS) $(UMLGRAPH_CLASS_SVGS) $(UMLGRAPH_SEQ_SVGS)
