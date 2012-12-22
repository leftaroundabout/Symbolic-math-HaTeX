all : shorttest0/shorttest0.pdf
.PHONY : all

RUNHS = runhaskell
TeXC = bin/pdflatex_halt-on-error.sh

shorttest0/shorttest0.pdf : HaTeX_SymMath_test0.hs
	$(RUNHS) $<
	$(TeXC) -output-directory shorttest0 shorttest0.tex
	mv shorttest0.tex shorttest0
