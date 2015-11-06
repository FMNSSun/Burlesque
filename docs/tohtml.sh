#!/bin/sh

pandoc -s -c doc.css -f markdown -t html BLSQ.md > BLSQ.html
