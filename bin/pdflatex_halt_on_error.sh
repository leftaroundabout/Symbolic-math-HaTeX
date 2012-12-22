#!/bin/bash

# Simple wrapper around pdflatex, so it doesn't completely spam the terminal
# even on successful runs and, when there are problems, to make them properly
# visible by means of colorisation.

recfile=/tmp/pdflatex_stdorec

pdflatex -halt-on-error $1 > $recfile
exitc=$?

RED=`echo -e '\033[01;31m'`
AMBER=`echo -e '\033[03;33m'`
RESET=`echo -e '\033[0m'`

if [ $exitc -ne 0 ]
then
  sed "s/^\!/${RED}&${RESET}/g;s/error/${RED}&${RESET}/gI;s/fatal/${RED}&${RESET}/gI" $recfile
  exit $exitc
fi

grep -i '\<warning\>\|^!' $recfile > /dev/null
warningexc=$?
if [ $warningexc -eq 0 ]
then
# Chances are a warnings were just because of changed labels, in which case
# re-running pdfLaTeX should fix the issue. Only output the warnings of this trial.
  pdflatex -halt-on-error $1 | grep -i '\<warning\>\|^!' | sed "s/warning:/${AMBER}&${RESET}/gI"
fi
exit 0
