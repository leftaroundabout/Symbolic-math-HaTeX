all : shorttest0/shorttest0.pdf
.PHONY : all

RUNHS = runhaskell
TeXC = bin/pdflatex_halt_on_error

shorttest0/shorttest0.pdf : HaTeXtest0.hs
	$(RUNHS) $<
	mv shorttest0.tex shorttest0
	cd shorttest0 && $(TeXC) shorttest0.tex
