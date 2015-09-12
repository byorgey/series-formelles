{-# LANGUAGE NoMonomorphismRestriction #-}

import           Diagrams
import           Diagrams.Backend.PGF.CmdLine
import           Diagrams.Prelude

main = mainWith (figure1 # frame 0.1)
