module Data.Tree.Wavelet ( -- * Alphabet trees
                           AlphaTree
                         , alphabeticTree
                           -- * Wavelet trees
                         , WaveletTree
                         , treeLabel
                         , prune
                         , access
                         , rank
                         , select
                         , rangeCount
                           -- * Utilities
                         , Interval(..)
                           -- * Debugging
                         , showWaveletTree
                         ) where

import Data.Monoid
import Data.Foldable
import Data.Tree.Binary
import qualified Data.Set as S

newtype WaveletTree a = WTree (BiTree [Bool] a)
                      deriving (Show)

newtype AlphaTree a = ATree (BiTree (S.Set a, S.Set a) a)

alphabeticTree :: Ord a => BiTree () a -> AlphaTree a
alphabeticTree = ATree . fst . mapWalk S.singleton (\(l,r) _ -> (l,r))

treeLabel :: Ord a => AlphaTree a -> [a] -> WaveletTree a
treeLabel (ATree alphabet) = WTree . go alphabet
  where go _ [] = Nil
        go (Leaf a) s = Leaf a
        go (Branch (ul,ur) l r) s =
          let s' = map (`S.member` ur) s
              l' = go l (filter (`S.member` ul) s)
              r' = go r (filter (`S.member` ur) s)
          in Branch s' l' r'
        go Nil _ = Nil

rankList :: Eq a => a -> [a] -> Int
rankList a = Data.Foldable.foldl' (flip g) 0
  where g x | x==a      = (+1)
            | otherwise = id

selectList :: Eq a => Int -> a -> [a] -> Maybe Int
selectList n a = go 0 . zip [0..]
  where go i [] = Nothing
        go i ((p,x):xs) =
          let i' = if x == a then i+1 else i
          in if n==i then Just p
                     else go i' xs

access :: Show a => Int -> WaveletTree a -> Maybe a
access i (WTree tree) = go i tree
  where go i (Leaf a)         = Just a
        go i  Nil             = Nothing
        go i (Branch lbl l r)
          | lbl !! i          = go (rankList True (take i lbl)) r
          | otherwise         = go (rankList False (take i lbl)) l
          where xs ! idx = xs !! (idx `mod` length xs)

rank :: Eq a => Int -> a -> WaveletTree a -> Int
rank i c (WTree tree) = go i tree
  where go i (Leaf a)                        = i
        go i (Branch lbl l r)
          | inLabels c l = go (rankList False (take i lbl)) l
          | otherwise    = go (rankList True (take i lbl)) r

prune :: WaveletTree a -> WaveletTree a
prune (WTree tree) = WTree $ go tree
  where go (Branch lbl l (Branch _ r Nil)) = go $ Branch lbl (go l) (go r)
        go (Branch lbl l (Branch _ Nil r)) = go $ Branch lbl (go l) (go r)
        go (Branch lbl (Branch _ Nil l) r) = go $ Branch lbl (go l) (go r)
        go (Branch lbl (Branch _ l Nil) r) = go $ Branch lbl (go l) (go r)
        go a = a

select :: Eq a => Int -> a -> WaveletTree a -> Maybe Int
select i c (WTree tree) = go i tree
  where go i (Leaf a)   = Just i
        go i (Branch lbl l r)
          | inLabels c l = do
            n <- go i l
            selectList n False lbl
          | otherwise = do
            n <- go i r
            selectList n True lbl

rangeCount :: (Ord a, Eq a)
           => Int -> Int -> Interval a -> WaveletTree a -> Int
rangeCount xs xe rng (WTree tree) = go xs xe tree
  where go _ _ Nil = error "rangeCount: Unexpected Nil"
        go xs xe tree
          | xs > xe                  = 0
          | rng `disjoint` bounds    = 0
          | rng `containsEq` bounds  = xe - xs + 1
          where Just bounds = labelsBounds tree
        go xs xe (Branch lbl l r) =
          let xls = rankList False (take (xs - 1) lbl) + 1
              xle = rankList False (take xe lbl)
              xrs = xs - xls
              xre = xe - xle
          in go xls xle l + go xrs xre r

-- | Intervals are inclusive
newtype Interval a = Interval (a,a)
                   deriving (Show, Eq)

instance Functor Interval where
    fmap f (Interval (a,b)) = Interval (f a, f b)

disjoint :: Ord a => Interval a -> Interval a -> Bool
disjoint (Interval (xs,xe)) (Interval (ys,ye))
  | ys > xe  = True
  | xs > ye  = True
  | otherwise = False

containsEq :: (Ord a, Eq a) => Interval a -> Interval a -> Bool
containsEq (Interval (xs,xe)) (Interval (ys,ye))
  | ys >= xs && ye <= xe = True
  | otherwise            = False

inLabels :: Eq a => a -> BiTree b a -> Bool
inLabels c (Leaf a)  = a == c
inLabels c tree      = getAny $ foldMap (Any . (==c)) tree

-- | Monoidal computation of the minimum and maximum element
newtype Range a = Range { getRange :: Maybe (a,a) }
                deriving (Show)

instance Ord a => Monoid (Range a) where
    mempty = Range Nothing
    Range Nothing `mappend` a = a
    a `mappend` Range Nothing = a
    Range (Just (a,b)) `mappend` Range (Just (c,d)) = Range (Just (min a c, max b d))

range :: a -> Range a
range x = Range $ Just (x,x)

labelsBounds :: Ord a => BiTree b a -> Maybe (Interval a)
labelsBounds tree = Interval `fmap` getRange (foldMap range tree)

showWaveletTree :: Show a => WaveletTree a -> String
showWaveletTree (WTree tree) = showBiTree tree
