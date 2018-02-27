{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module SpeciesDiagrams where

import           Control.Arrow                 (first, second)
import           Data.Colour.Palette.BrewerSet
import           Data.List                     (intersperse, permutations)
import           Data.List.Split
import qualified Data.Map                      as M
import           Data.Maybe                    (fromJust, fromMaybe)
import           Data.Tree
import           Diagrams.Backend.PGF.CmdLine
import           Diagrams.Prelude              hiding (Empty, parts)
import           Diagrams.TwoD.Layout.Tree
import           Graphics.SVGFonts
import qualified Math.Combinatorics.Multiset   as MS

colors :: [Colour Double]
colors = brewerSet Set1 9

labR, arrowGap :: Double
labR     = 0.3
arrowGap = 0.2

aLabels :: [Diagram B]
aLabels = map (sized (dims2D (4*labR) (4*labR)))
  [ circle 1
  , triangle 1
  , square 1
  , pentagon 1
  , rect 1 1.618
  , rect 1.618 1
  , circle 1 # scaleX 1.618
  , circle 1 # scaleY 1.618
  ]

type EdgeLabel = P2 Double -> P2 Double -> Diagram B

sLabels :: [EdgeLabel]
sLabels =
  [ connStyle mempty
  , connStyle $ (mempty # lw veryThick)
  , connStyle $ (mempty # dashingG [0.1,0.1] 0)
  , connStyle $ (mempty # dashingG [0.05,0.15] 0)
  , connStyle $ (mempty # dashingG [0.05,0.05,0.1,0.05] 0)
  , \p q -> let v = 0.03 *^ normalize (perp (q .-. p))
            in ((p .+^ v) ~~ (q .+^ v)) <> ((p .-^ v) ~~ (q .-^ v))
  ]
  where
    connStyle sty p q = (p ~~ q) # applyStyle sty
    perp = rotateBy (1/4)

labSty :: Int -> Maybe EdgeLabel
labSty i = Just (sLabels !! i)

leafData :: Int -> Diagram B
leafData i = (aLabels !! i) # sized (dims2D labR labR) # fc black <> square (labR*1.5) # fc white

-- text' :: Double -> String -> Diagram B
-- text' d s = (stroke $ textSVG' (TextOpts lin INSIDE_H KERN False d d) s) # fc black # lw none

text' :: Double -> String -> Diagram B
text' d s = text ("$" ++ s ++ "$") # fontSizeL d

labT :: Int -> Diagram B
labT n = text' 1.5 (show n) # scale labR <> lab n

lab :: Int -> Diagram B
lab n = lab' (colors !! n)

lab' :: (TrailLike b, Transformable b, HasStyle b, V b ~ V2, N b ~ Double) => Colour Double -> b
lab' c = circle labR
       # fc white
       # lc c
       # lwG (labR / 5)

cyc :: [Int] -> Double -> Diagram B
cyc labs r = cyc' (map lab labs) r

cyc' :: (Monoid' a, TrailLike a, Transformable a, HasStyle a, HasOrigin a, V a ~ V2, N a ~ Double) => [a] -> Double -> a
cyc' labs r
  = mconcat
  . zipWith (\l (p,a) -> l # moveTo p <> a) labs
  $ zipWith rotateBy
      [1/4, 1/4 + 1/(fromIntegral n) .. ]
      (map mkLink labs)
 where
  n = length labs
  mkLink _ = ( origin # translateX r
             ,
               ( arc (angleDir startAngle) arcAngle
                 # scale r
                 <>
                 eqTriangle 0.1
                 # translateX r
                 # rotate (startAngle ^+^ arcAngle)
                 # fc black
               )
             )
  startAngle = ((labR + arrowGap)/r) @@ rad
  arcAngle   = ((tau/fromIntegral n) @@ rad) ^-^ (2 *^ startAngle)


newtype Cyc a = Cyc {getCyc :: [a]}
  deriving Functor

data Pointed a = Plain a | Pointed a

class Drawable d where
  draw :: d -> Diagram B

instance Drawable (QDiagram PGF V2 Double Any) where
  draw = id

instance Drawable a => Drawable (Cyc a) where
  draw (Cyc ls) = cyc' (map draw ls # sized (mkWidth (labR*2))) 1

instance Drawable a => Drawable [a] where
  draw ls = centerX . hcat' (with & sep .~ 0.1)
          $ intersperse (mkArrow 0.5 mempty) (map draw ls)

instance Drawable a => Drawable (Pointed a) where
  draw (Plain a) = draw a
  draw (Pointed a) = point (draw a)

point :: Diagram B -> Diagram B
point d = d <> drawSpN Hole # sizedAs (d # scale 5)

down :: Cyc (Diagram B) -> Cyc (Cyc (Pointed (Diagram B)))

down (Cyc ls) = Cyc (map Cyc (pointings ls))

pointings :: [a] -> [[Pointed a]]
pointings []     = []
pointings (x:xs) = (Pointed x : map Plain xs) : map (Plain x :) (pointings xs)

elimArrow :: Diagram B
elimArrow = hrule 2
        ||| eqTriangle 0.2 # rotateBy (-1/4) # fc black

mkArrow :: Double -> Diagram B -> Diagram B
mkArrow len l =
  ( l
    ===
    (arrow len # translateX (-len/2) <> rect len 0.5 # lw none)
  )
  # alignB

data SpN = Lab (Either Int String)
         | Leaf (Maybe (Diagram B))
         | Hole
         | Point
         | Sp (Diagram B) (Angle Double)
         | Bag

type SpT = Tree (Maybe EdgeLabel, SpN)

drawSpT' :: T2 Double -> SymmLayoutOpts Double (Maybe EdgeLabel, SpN) -> SpT -> Diagram B
drawSpT' tr slopts
  = transform tr
  . renderTree' (drawSpN' (inv tr) . snd) drawSpE
  . symmLayout' slopts

drawSpT :: SpT -> Diagram B
drawSpT = drawSpT' (rotation (1/4 @@ turn))
                   (with & slHSep .~ 0.5 & slVSep .~ 2 & slWidth .~ slw)
  where
    slw (_, Leaf (Just d)) = (-width d/2, width d/2)
    slw (_, sp@(Sp _ _)) = let w = width (drawSpN' (rotation (1/4 @@ turn)) sp)
                           in  (-w/2, w/2)
    slw _ = (0,0)

drawSpN' :: T2 Double -> SpN -> Diagram B
drawSpN' _  (Lab (Left n))    = lab n # scale 0.5
drawSpN' tr (Lab (Right t))   = (drawSpN' tr (Leaf Nothing) ||| strutX (labR/2) ||| text' 0.3 t) # transform tr
drawSpN' _  (Leaf Nothing)  = circle (labR/2) # fc black
drawSpN' _  (Leaf (Just d)) = d
drawSpN' _  Hole              = circle (labR/2) # lwG (labR / 10) # fc white
drawSpN' tr Point             = drawSpN' tr (Leaf Nothing) <> drawSpN' tr Hole # scale 1.7
drawSpN' tr (Sp s f) = ( arc (angleDir ((3/4 @@ turn) ^-^ f^/2)) ((3/4 @@ turn) ^+^ f^/2) # scale 0.3
                       |||
                       strutX 0.1
                       |||
                       s # transform tr
                       )
drawSpN' _  Bag     =
                ( text' 1 "{" # scale 0.5 ||| strutX (labR/4)
                  ||| circle (labR/2) # fc black
                  ||| strutX (labR/4) ||| text' 1 "}" # scale 0.5
                ) # centerX

drawSpN :: SpN -> Diagram B
drawSpN = drawSpN' mempty

drawSpE :: (t, P2 Double) -> ((Maybe EdgeLabel, SpN), P2 Double) -> Diagram B
drawSpE (_,p) ((_,Hole),q) = (p ~~ q) # dashingG [0.05,0.05] 0
drawSpE (_,p) ((Just f,_), q) = f p q
drawSpE (_,p) (_,q) = p ~~ q

nd :: Diagram B -> Forest (Maybe EdgeLabel, SpN) -> SpT
nd x = Node (Nothing, Sp x (1/3 @@ turn))

nd' :: EdgeLabel -> Diagram B -> Forest (Maybe EdgeLabel, SpN) -> SpT
nd' l x = Node (Just l, Sp x (1/3 @@ turn))

lf :: a -> Tree (Maybe EdgeLabel, a)
lf x = Node (Nothing, x) []

lf' :: EdgeLabel -> a -> Tree (Maybe EdgeLabel, a)
lf' l x = Node (Just l, x) []

struct :: Int -> String -> Diagram B
struct n x = drawSpT (struct' n x)
           # centerXY

struct' :: Int -> String -> SpT
struct' n x = struct'' n (text' 1 x <> rect 2 1 # lw none)

struct'' :: Int -> Diagram B -> SpT
struct'' n d = nd d (replicate n (lf (Leaf Nothing)))

linOrd :: [Int] -> Diagram B
linOrd ls =
    connect' (with & arrowHead .~ noHead) "head" "last"
  . hcat' (with & sep .~ 0.5)
  $ map labT ls & _head %~ named "head" & _last %~ named "last"

unord :: _ => [b] -> b
unord [] = circle 1 # lc gray
unord ds = elts # centerXY
           <> roundedRect w (mh + s*2) ((mh + s*2) / 5)
  where
    elts  = hcat' (with & sep .~ s) ds
    mw    = maximum' 0 . map width  $ ds
    s     = mw * 0.5
    mh    = maximum' 0 . map height $ ds
    w     = ((fromIntegral (length ds + 1) * s) +) . sum . map width $ ds
    maximum' d [] = d
    maximum' _ xs = maximum xs

enRect' :: (Semigroup a, TrailLike a, Alignable a, Enveloped a, HasOrigin a, V a ~ V2, N a ~ Double) => Double -> a -> a
enRect' o d = roundedRect (width d + o) (height d + o) o <> d # centerXY

enRect :: (Semigroup a, TrailLike a, Alignable a, Enveloped a, HasOrigin a, V a ~ V2, N a ~ Double) => a -> a
enRect = enRect' 0.5

-- txt x = text x # fontSizeO 10 <> square 1 # lw none
txt = txt' 10

txt' d x = text ("$" ++ x ++ "$") # fontSizeO d <> square 1 # lw none

------------------------------------------------------------
-- Some specific constructions

mlocColor = blend 0.5 white lightblue
eltColor = blend 0.5 white lightgreen

mloc m = text (show m) <> circle 0.8 # fc mlocColor
elt x = text (show x) <> square 1.6 # fc eltColor

arm typ m n r = ( typ m # rotateBy (-r)
              ||| hrule 1.5
              ||| typ n # rotateBy (-r)
                )
                # translateX 3
                # rotateBy r

arms typ elts = zipWith (\[e1,e2] r -> arm typ e1 e2 r) (chunksOf 2 elts) [1/8 + 0.001, 1/8+0.001 +1/4 .. 1]

octo' typ elts = (mconcat (arms typ elts) <> circle 3)

octo = octo' mloc

sampleT7 = Node 3 [Node 4 (map lf [2,1,6]), Node 5 [], Node 0 [lf 7]]
  where
    lf x = Node x []

tree :: Diagram B
tree = renderTree
         mloc
         (~~)
         (symmLayout' (with & slHSep .~ 4 & slVSep .~ 4) sampleT7)

drawBinTree' :: SymmLayoutOpts Double (Diagram B) -> BTree (Diagram B) -> Diagram B
drawBinTree' opts
  = maybe mempty (renderTree id (~~))
  . symmLayoutBin' opts

drawBinTree :: BTree (Diagram B) -> Diagram B
drawBinTree = drawBinTree' with

drawBinTreeWide :: BTree (Diagram B) -> Diagram B
drawBinTreeWide = drawBinTree' (with & slHSep .~ 1.5)

select :: [a] -> [(a,[a])]
select [] = []
select (a:as) = (a,as) : (map . second) (a:) (select as)

subsets :: [a] -> [([a],[a])]
subsets [] = [([],[])]
subsets (a:as) = (map . first) (a:) s ++ (map . second) (a:) s
  where s = subsets as

type Edge = (Int,Int)
type Graph = (M.Map Int (P2 Double), [Edge])

drawGraph drawLoc (locs, edges) = drawLocs <> drawEdges
  where
    drawLocs  = mconcat . map (\(n,p) -> drawLoc n # moveTo p) . M.assocs $ locs
    drawEdges = mconcat . map drawEdge $ edges
    drawEdge (i1,i2) = lkup i1 ~~ lkup i2
    lkup i = fromMaybe origin $ M.lookup i locs

gr :: Diagram B
gr  = drawGraph mloc
         ( M.fromList
           [ (0, 3 ^& (-1))
           , (1, 8 ^& 0)
           , (2, origin)
           , (3, 8 ^& 2)
           , (4, 4 ^& 2)
           , (5, 3 ^& (-3))
           ] # scale 1.5
         , [(2,0), (2,4), (0,4), (4,3), (3,1), (0,1), (0,5)]
         )

--------------------------------------------------

sampleBTree5, sampleBTree7 :: BTree Int
sampleBTree5 = (BNode (0 :: Int) (BNode 1 Empty Empty) (BNode 2 (BNode 3 Empty Empty) (BNode 4 Empty Empty)))
sampleBTree7 = (BNode (0 :: Int) (BNode 1 (BNode 2 Empty (BNode 3 Empty Empty)) Empty) (BNode 4 (BNode 5 Empty Empty) (BNode 6 Empty Empty)))


wideTree
  :: (Monoid m, Semigroup m, TrailLike (QDiagram b V2 Double m ))
  => (a -> QDiagram b V2 Double m) -> BTree a -> QDiagram b V2 Double m
wideTree n
  = maybe mempty (renderTree n (~~))
  . symmLayoutBin' (with & slVSep .~ 4 & slHSep .~ 6)

mkLeaf
  :: ( Semigroup m, IsName n)
  => QDiagram b V2 Double m -> n -> QDiagram b V2 Double m
mkLeaf shp n = shp # fc white # named n

numbered :: Show a => a -> Diagram B
numbered n = mkLeaf (text (show n) # fc black <> circle 1) ()

lettered :: Int -> Diagram B
lettered n = mkLeaf (text [['a' ..] !! n] # fc black <> circle 1) ()

drawList nd n = drawList' nd [0::Int .. (n - 1)]

drawList' nd ns = lst # centerX `atop` hrule (width lst - w)
  where
    elts = map nd ns
    w    = maximum . map width $ elts
    lst  = hcat' (with & sep .~ w/2) elts

enumTrees :: [a] -> [BTree a]
enumTrees []   = [ Empty ]
enumTrees xxs  = [ BNode x l r
             | (x,xs) <- select xxs
             , (ys, zs) <- subsets xs
             , l <- enumTrees ys
             , r <- enumTrees zs
             ]

tag :: Int -> Diagram B -> Diagram B
tag i d = d # centerXY <> roundedRect w h r # applyStyle (tagStyles !! i)
  where
    w = width d + 1
    h = height d + 1
    r = 0.5


tagStyles :: [Style V2 Double]
tagStyles = cycle
  [ mempty
  , mempty # lw veryThick # lc green
  , mempty # lw veryThick # lc green # dashingG [0.1,0.1] 0
  ]

--------------------------------------------------

enclose :: Double -> Double -> Diagram B -> Diagram B
enclose g r d = d # centerXY <> roundedRect (width d + g) (height d + g) r

objs :: IsName n => [n] -> Diagram B
objs = enclose 1 1 . vcat' (with & sep .~ 1.5) . (map (\n -> dot # named n))
  where
    dot = circle 0.1 # fc black

--------------------------------------------------
-- Partial bijections

data PBij a b where
  PBij :: [a] -> (a -> b) -> PBij a b

applyPBij :: PBij a b -> (a -> b)
applyPBij (PBij _ f) = f

pbComp :: PBij b c -> PBij a b -> PBij a c
pbComp (PBij _ f) (PBij as g) = PBij as (f . g)

fromRel :: Eq a => [(a,b)] -> PBij a b
fromRel rs = PBij (map fst rs) (fromJust . flip lookup rs)

drawPBij :: (IsName a, IsName b) => PBij a b -> Diagram B -> Diagram B
drawPBij (PBij as f) = applyAll [ conn a (f a) | a <- as ]
  where
    conn x y = connect' pBijAOpts x y

pBijAOpts = with & arrowTail .~ spike' & gaps .~ local 0.3 & lengths .~ local 0.3

mkSet = mkSet' (const dot)
  where
    dot = circle 0.2 # fc black

mkSet' dn names
  = enclose 0.5 1
  . vcat' (with & sep .~ 0.5)
  . zipWith named names
  . map dn
  $ names



pb1 :: PBij Int Char
pb1 = fromRel
  [ (0, 'd')
  , (1, 'a')
  , (2, 'b')
  , (3, 'e')
  ]

pb2 :: PBij Int Int
pb2 = fromRel [ (100, 3), (101, 2) ]

------------------------------------------------------------------------

parts :: [a] -> [[[a]]]
parts = map (map MS.toList . MS.toList) . MS.partitions . MS.fromDistinctList

cycles [] = []
cycles (x:xs) = map (x:) (permutations xs)

perms :: [a] -> [[[a]]]
perms = concatMap (mapM cycles) . parts

drawPerm = hcat' (with & sep .~ 0.2) . map ((\l -> cyc' l 0.8) . map labT)

smallHoleNode = circle labR # fc white # dashingL [0.05,0.05] 0
holeNode = (circle 0.8 # fc white # dashingL [0.1,0.1] 0)

fun x y = hcat' (with & sep .~ 1)
  [ x # centerY
  , arrow 3
  , y # centerY
  ]

------------------------------------------------------------

mkNamedNode :: IsName n => (Int -> n) -> (Int -> String) -> Int -> Diagram B
mkNamedNode name sh n = (text (sh n) # scale labR <> lab n) # named (name n)

mkNamedTree :: IsName n => (Int -> n) -> (Int -> String) -> BTree Int -> BTree (Diagram B)
mkNamedTree name sh = fmap (mkNamedNode name sh)

