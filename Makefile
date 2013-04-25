all : shorttest0/shorttest0.pdf
.PHONY : all

RUNHS = runhaskell
TeXC = bin/pdflatex_halt-on-error.sh

libTeXMyMath = dist/build/HSTeX-my-math-0.1.o
libTeXMyMath_src = Math/LaTeX/*.hs Math/LaTeX/Internal/*.hs


shorttest0/shorttest0.pdf : shorttest0.hs $(libTeXMyMath)
	$(RUNHS) $<
	@rm shorttest0/*
	$(TeXC) -output-directory shorttest0 shorttest0.tex
	mv shorttest0.tex shorttest0

$(libTeXMyMath) : $(libTeXMyMath_src)
	cabal install