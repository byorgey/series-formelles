{-# LANGUAGE NoMonomorphismRestriction #-}

import           Data.Tree
import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Prelude

-- Pre: subtrees are always arranged in counterclockwise order around root
drawEnrichedTree :: Tree (P2 Double) -> Diagram B
drawEnrichedTree = drawEnrichedTree' Nothing

drawEnrichedTree' :: Maybe (P2 Double) -> Tree (P2 Double) -> Diagram B
drawEnrichedTree' mParent (Node root ts) = edges <> mconcat subTrees <> arcR
  where
    edges = mconcat [ root ~~ rootLabel t | t <- ts ]
    subTrees = map (drawEnrichedTree' (Just root)) ts
    arcR = arc dStart (angleBetweenDirs dStart dEnd)
         # scale arcRadius # moveTo root
      where
        dStart = head dirs # rotateBy (-overhang)
        dEnd   = last dirs # rotateBy overhang
        dirs   = case (ts, mParent) of
                   ([], Nothing)     -> [xDir]
                   ([], Just parent) -> [dirBetween root parent]
                   _                 -> map (flip dirBetween root . rootLabel) ts

        overhang  = if null ts then 1/5 else 1/20
        arcRadius = if null ts then 0.05 else 0.2

t :: Tree (P2 Double)
t = Node origin [Node (1 ^& (-1)) [], Node (1 ^& 1) [Node (2 ^& 1) [], Node (2 ^& 2) []]]

main = mainWith (drawEnrichedTree t # frame 0.5)
