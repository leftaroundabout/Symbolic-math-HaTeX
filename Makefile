all : shorttest0/shorttest0.pdf
.PHONY : all

RUNHS = runhaskell
TeXC = bin/pdflatex_halt-on-error.sh

shorttest0/shorttest0.pdf : shorttest0.hs
	$(RUNHS) $<
	@rm shorttest0/*
	$(TeXC) -output-directory shorttest0 shorttest0.tex
	mv shorttest0.tex shorttest0
