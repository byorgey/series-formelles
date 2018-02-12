#!/usr/bin/env stack
{- stack
   --resolver lts-9.21
   --install-ghc
   runghc
   --package diagrams-pgf-1.4
   --package multiset-comb-0.2.4.1
   --package texrunner-0.0.1.1
   --package lens-4.16
   --package palette-0.1.0.5
   --package split-0.2.3.3
   --package containers-0.5.11.0
   --package diagrams-core-1.4.0.1
   --package diagrams-lib-1.4.2
   --package diagrams-contrib-1.4.1
   --package SVGFonts-1.6.0.3
   --package shake-0.16
   --package diagrams-builder-0.8.0.2
-}

-- Tried using the above with Shake.hs but not sure it really works.
-- Need to be able to specify -fpgf flag to diagrams-builder, but
-- can't do that on stack command line, it seems.
