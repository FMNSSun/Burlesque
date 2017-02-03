#!/bin/sh

pandoc --toc -s -c doc.css -f markdown -t html BLSQ.md > BLSQ.html
pandoc --toc -s -f markdown -t latex BLSQ.md > BLSQ.latex
pandoc --toc -s -f markdown -t rtf BLSQ.md > BLSQ.rtf
pandoc --toc -s -f markdown -t odt BLSQ.md -o BLSQ.odt
pdflatex BLSQ.latex && pdflatex BLSQ.latex
