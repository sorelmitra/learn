VPATH=..

DITAA=/usr/local/bin/ditaa

DITAAS:= \
	test.txt \

DITAA_PNGS:=$(DITAAS:.txt=.png)

all: $(DITAA_PNGS)

%.png: %.txt
	${DITAA} $< $@

clean:
	rm -fv $(DITAA_PNGS)
