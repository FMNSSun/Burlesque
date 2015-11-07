#!/bin/sh

pandoc --toc -s -c doc.css -f markdown -t html BLSQ.md > BLSQ.html
pandoc --toc -s -f markdown -t latex BLSQ.md > BLSQ.latex
pdflatex BLSQ.latex && pdflatex BLSQ.latex
