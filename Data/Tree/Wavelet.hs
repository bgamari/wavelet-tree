module Data.Tree.Wavelet where

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

inLabels :: Eq a => a -> BiTree b a -> Bool
inLabels c (Leaf a)  = a == c
inLabels c tree      = getAny $ foldMap (Any . (==c)) tree
