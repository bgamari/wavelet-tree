import Data.Tree.Wavelet
import Data.Tree.Binary
import Data.Foldable
import Data.List

--string = "hello world"
string = "abracadabra"
alphabet = nub $ sort string
--atree = alphabeticTree $ foldMap Leaf alphabet
atree = alphabeticTree $
        Branch ()
          (Branch ()
            (Branch ()
              (Leaf 'a')
              (Leaf 'b')
            )
            (Leaf 'c')
          )
          (Branch ()
            (Leaf 'd')
            (Leaf 'r')
          )
wtree = treeLabel atree string

main = do
    putStrLn $ showWaveletTree $ prune wtree
    print $ prune wtree
    print $ map (\i->access i wtree) [0..length string-1]
    print $ rank 6 'a' wtree
    print $ select 2 'b' wtree
    print $ rangeCount 0 5 (Interval ('a','d')) wtree
    print $ rangeReport 0 5 (Interval ('a','d')) wtree
