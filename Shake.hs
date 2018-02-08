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

import           Development.Shake
import           Development.Shake.FilePath
import           System.Exit

lhs2TeX, pdflatex, rubber :: String
lhs2TeX  = "lhs2TeX"
pdflatex = "pdflatex"
rubber = "rubber"

main :: IO ()
main = shake shakeOptions $ do

  want ["series-formelles.pdf"]

  "*.pdf" %> \output -> do
      let input = replaceExtension output "tex"
      need [input]
      cmd pdflatex $ ["--enable-write18", input]
