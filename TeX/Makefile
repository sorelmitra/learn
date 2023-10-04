VPATH=..

XELATEX=/Library/TeX/texbin/xelatex

TEXES:= \
	test.tex \
	book.tex \
	booklet.tex \

PDFS:=$(TEXES:.tex=.pdf)

AUXES:=$(TEXES:.tex=.aux)
IDXES:=$(TEXES:.tex=.idx)
LOGS:=$(TEXES:.tex=.log)
OUTS:=$(TEXES:.tex=.out)
TOCS:=$(TEXES:.tex=.toc)

all: $(PDFS)

.DELETE_ON_ERROR:

booklet.pdf: book.tex

%.pdf %.aux %.idx: %.tex
	${XELATEX} $<
	@while grep 'Rerun to get ' $*.log ; do ${XELATEX} $< ; done
	@while grep 'Rerun LaTeX.' $*.log ; do ${XELATEX} $< ; done

clean:
	rm -fv $(PDFS) $(AUXES) $(IDXES) $(LOGS) $(OUTS) $(TOCS)
