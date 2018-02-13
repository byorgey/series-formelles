#!/bin/sh

stack build diagrams diagrams-pgf diagrams-builder palette SVGFonts multiset-comb shake
stack runghc Shake.hs
