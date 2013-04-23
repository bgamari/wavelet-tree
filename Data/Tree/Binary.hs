module Data.Tree.Binary where

import Prelude hiding (concat, concatMap)
import Control.Applicative
import Data.Foldable
import Data.Traversable
import Data.Monoid

-- | A binary tree with labels on branches
data BiTree label a = Branch !label !(BiTree label a) !(BiTree label a)
                    | Leaf !a
                    | Nil
              deriving (Show)

instance Functor (BiTree label) where
    fmap f (Branch label l r) = Branch label (fmap f l) (fmap f r)
    fmap f (Leaf a)           = Leaf (f a)
    fmap _ Nil                = Nil

instance Traversable (BiTree label) where
    traverse f (Branch label l r) = Branch label <$> traverse f l <*> traverse f r
    traverse f (Leaf a)           = Leaf <$> f a
    traverse f  Nil               = pure Nil

instance Foldable (BiTree label) where
    foldMap = foldMapDefault

instance Monoid label => Monoid (BiTree label a) where
    mempty = Nil
    l `mappend` r = Branch mempty l r

size :: BiTree label a -> Int
size = getSum . foldMap (const $ Sum 1)

-- | Start at the leaves and transform the labels with a monoidal accumulator
mapWalk :: (Monoid m)
        => (a -> m) -> ((m,m) -> label -> label') -> BiTree label a -> (BiTree label' a, m)
mapWalk _ _ Nil              = (Nil, mempty)
mapWalk f g (Leaf a)         = (Leaf a, f a)
mapWalk f g (Branch lbl l r) = let (l', ml) = mapWalk f g l
                                   (r', mr) = mapWalk f g r
                                   m = ml <> mr
                               in (Branch (g (ml,mr) lbl) l' r', m)

foldMapWithLabels :: (Monoid m)
                  => (label -> m) -> (a -> m) -> BiTree label a -> m
foldMapWithLabels f g (Leaf a)         = g a
foldMapWithLabels f g (Branch lbl l r) =
    f lbl <> foldMapWithLabels f g l <> foldMapWithLabels f g r

toDot :: (Show label, Show a) => BiTree label a -> String
toDot tree = "graph bitree {" ++ unlines (go "" tree) ++ "}"
  where go path (Branch lbl l r) = [ "b"++path++" [label=\""++show lbl++"\"];\n"
                                   , path ++ " -- " ++ 'l':path
                                   , path ++ " -- " ++ 'r':path
                                   ]
                                 ++ go ('l':path) l ++ go ('r':path) r
        go path (Leaf a)         = ["n"++path++" [label=\""++show a++"\"];\n"]
        go path Nil              = ["n"++path++" [label=\"nil\"];\n"]

showBiTree :: (Show label, Show a) => BiTree label a -> String
showBiTree = unlines . go
  where go (Branch lbl l r) = ["+ "++show lbl] ++ concatMap (\a->map ("  "++) $ go a) [l, r]
        go Nil              = ["0"]
        go (Leaf a)         = ["- "++show a]