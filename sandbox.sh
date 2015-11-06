#!/bin/sh

cabal sandbox init
cabal install diagrams diagrams-pgf -fpgf diagrams-builder shake
