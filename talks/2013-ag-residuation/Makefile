styles := $(wildcard *.sty)
pdfs := $(patsubst %.tex,%.pdf,$(wildcard *.tex))

all: $(pdfs)

clean:
	rm -rf *.aux *.dvi *.log *.nav *.out *.ptb *.snm *.toc *.vrb *~ # latex

cleanall: clean
	rm -rf *.pdf

handout.pdf: handout.tex talk.tex $(styles)
	pdflatex $<
	pdflatex $<

%.pdf: %.tex $(styles)
	pdflatex $<
	pdflatex $<
