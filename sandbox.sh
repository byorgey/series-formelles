#!/bin/sh

cabal sandbox init
cabal install diagrams diagrams-pgf -fpgf diagrams-builder palette SVGFonts multiset-comb shake
