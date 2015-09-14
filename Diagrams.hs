{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Diagrams where

import           Control.Monad
import           Data.Tree
import           Diagrams.Backend.PGF.CmdLine
import           Diagrams.Prelude
import           Diagrams.TwoD.Path.Metafont

------------------------------------------------------------
-- Drawing enriched trees (Fig 0)

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

------------------------------------------------------------
-- Figure 1

arrowedge :: P2 Double -> P2 Double -> Diagram B
arrowedge p q = arrowBetween' speciesArrowOpts p (lerp 0.4 p q) <> (lerp 0.5 p q ~~ q)

speciesHeadLength = 0.07
speciesArrowOpts = with & headLength .~ local speciesHeadLength

vertexDot :: Diagram B
vertexDot = circle 0.01 # fc black

fixpt :: Double -> Diagram B
fixpt r = mconcat
  [ vertexDot
  , arrowAt' speciesArrowOpts
      (origin # translateY vOffset
              # translateX (- 0.5 * speciesHeadLength)
      )
      (0.001 *^ unit_X)
  , circle r # translateY r
  ]
  where
    vOffset | speciesHeadLength < 2*r = r + sqrt (r*r - (speciesHeadLength/2)^2)
            | otherwise               = 2*r

figure1 :: Diagram B
figure1 = mconcat
  [ mconcat . map mkLoop $
    [ pentagon 0.4 # rotateBy (1/2) # translate ((-0.3) ^& 0.5)
    , heptagon 0.3 # translateX (-1.3)
    , square 0.4 # shearX (-0.3) # translate ((-0.6) ^& (-0.4))
    , triangle 0.2 # rotateBy (1/2)
    , triangle 0.3 # rotateBy (1/5) # translate (0.4 ^& 0.4)
    ]
  , mconcat $ map (place (fixpt 0.08))
    [ (1 ^& 0.3)
    , (1.5 ^& 0)
    , (0.6 ^& (-0.3))
    , (0.2 ^& (-0.4))
    , (1.25 ^& (-0.45))
    , (0 ^& (-0.7))
    , (0.4 ^& (-0.85))
    ]
  , yinyang
  ]
  where
    outside :: Located (Trail V2 Double)
    outside = ellipseXY 1.8 1
    s, t :: Double
    s = 0.2
    t = 0.75
    p, q :: P2 Double
    p = outside `atParam` s
    q = outside `atParam` t

    yinyang :: Diagram B
    yinyang = mconcat
      [ metafont $ p
                     .- leaving (negated $ outside `tangentAtParam` s) -.
                   lerp 0.5 p q
                     .- arriving (outside `tangentAtParam` t) -.
                   endpt q
      , stroke outside
      ]

    mkLoop :: [P2 Double] -> Diagram B
    mkLoop ps = mconcat $
      (zipWith arrowedge <*> (\xs -> tail xs ++ [head xs])) ps
      ++
      map (place vertexDot) ps

-- main = mainWith (drawEnrichedTree (layoutEnrichedTree t2) # frame 0.5)
