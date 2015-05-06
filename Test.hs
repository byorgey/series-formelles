{-# LANGUAGE NoMonomorphismRestriction #-}

import           Data.Tree
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

drawEnrichedTree :: Tree (P2 Double) -> Diagram B
drawEnrichedTree (Node p ts) = edges <> mconcat ds <> arcR
  where
    edges = mconcat [ p ~~ rootLabel t | t <- ts ]
    ds = map drawEnrichedTree ts
    arcR = mempty

