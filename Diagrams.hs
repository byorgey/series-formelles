{-# LANGUAGE NoMonomorphismRestriction #-}

module Diagrams where

import           Data.Tree
import           Diagrams.Backend.PGF.CmdLine
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

layoutEnrichedTree :: Tree (Angle Double, Angle Double) -> Tree (P2 Double)
layoutEnrichedTree = layoutEnrichedTree' origin unit_Y

layoutEnrichedTree' :: P2 Double -> V2 Double -> Tree (Angle Double, Angle Double) -> Tree (P2 Double)
layoutEnrichedTree' root dir (Node (sep, off) ts) = Node root ts'
  where
    n = length ts
    firstChildP = (root .+^ dir) # rotateAround root (off ^+^ ((fromIntegral (n-1) *^ sep) ^/ (-2)))
    childPs = iterateN n (rotateAround root sep) firstChildP
    ts' = zipWith (\p -> layoutEnrichedTree' p (p .-. root)) childPs ts

type ET = Tree (Angle Double, Angle Double)

lf :: ET
lf = Node (0 @@ turn, 0 @@ turn) []

nd :: [ET] -> ET
nd = Node (20 @@ deg, 0 @@ turn)

nudge :: Angle Double -> ([ET] -> ET) -> ([ET] -> ET)
nudge a f ts = nudge' a (f ts)
  where
    nudge' a (Node (sep,off) ts) = Node (sep, off ^+^ a) ts

widen :: Angle Double -> ([ET] -> ET) -> ([ET] -> ET)
widen a f ts = widen' a (f ts)
  where
    widen' a (Node (sep,off) ts) = Node (sep ^+^ a, off) ts

figure0 :: ET
figure0 =
  (nd # widen (5 @@ deg) # nudge (9/40 @@ turn))
  [ lf
  , (nd # nudge (40 @@ deg))
    [ lf
    , lf
    , (nd # nudge (-20 @@ deg))
      [ lf
      , lf
      , nd
        [ lf
        , lf
        , (nd # nudge (-30 @@ deg))
          [ lf
          , lf
          ]
        ]
      , lf
      ]
    ]
  , lf
  , lf
  , (nd # nudge (-30 @@ deg))
    [ lf
    , nd
      [ lf
      , nd [ lf, lf ]
      , lf
      ]
    , lf
    ]
  , lf
  ]

-- main = mainWith (drawEnrichedTree (layoutEnrichedTree t2) # frame 0.5)
