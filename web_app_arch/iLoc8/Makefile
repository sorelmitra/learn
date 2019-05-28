VPATH=..

PLANTUML=/usr/local/bin/plantuml

XELATEX=/Library/TeX/texbin/xelatex

PLANTUMLS:= \
	flow_dashboard_web.pu \
	architecture_adaptors.pu \
	architecture_adaptor_media.pu \
	architecture_birds_eye.pu \
	architecture_components.pu \
	architecture_server_api.pu \
	usecase_agent_back.pu \
	usecase_agent_transfer.pu \
	usecase_general.pu \
	usecase_start.pu \
	usecase_features.pu \

PLANTUML_SVGS:=$(PLANTUMLS:.pu=.svg)
PLANTUML_PNGS:=$(PLANTUMLS:.pu=.png)

TEXES:= \
	iLoc8_HLD.tex \

PDFS:=$(TEXES:.tex=.pdf)

AUXES:=$(TEXES:.tex=.aux)
IDXES:=$(TEXES:.tex=.idx)
LOGS:=$(TEXES:.tex=.log)
OUTS:=$(TEXES:.tex=.out)
TOCS:=$(TEXES:.tex=.toc)

all: pngs $(PDFS)

svgs: $(PLANTUML_SVGS)

pngs: $(PLANTUML_PNGS) 

$(PDFS): $(PLANTUML_PNGS)

%.svg: %.pu
	${PLANTUML} -tsvg $< $@

%.png: %.pu
	${PLANTUML} -tpng $< $@

.DELETE_ON_ERROR:

%.pdf %.aux %.idx: %.tex
	${XELATEX} $<
	@while grep 'Rerun to get ' $*.log ; do ${XELATEX} $< ; done
	@while grep 'Rerun LaTeX.' $*.log ; do ${XELATEX} $< ; done

clean_pu:
	rm -fv $(PLANTUML_SVGS) $(PLANTUML_PNGS)

clean_tex:
	rm -fv $(PDFS) $(AUXES) $(IDXES) $(LOGS) $(OUTS) $(TOCS)
