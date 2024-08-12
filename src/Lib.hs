{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib where

-- import Lib
import Control.Monad
import Data.Tree
import Data.Maybe
import Data.List

-- we tend to see [Int] as a unique path on a tree 
type Path = [Int] 

-- enumerates based on list length
enum :: [a] -> Path 
enum xs = [x | x <- [1..(length xs)]]

-- diminishes each integer by 1
diminishEach :: Path -> Path
diminishEach xs= (+(-1)) <$> xs

-- gets an element from maybe list according to given index
indexMaybe :: Maybe [a] -> Int -> Maybe a
indexMaybe Nothing _   = Nothing
indexMaybe (Just xs) i
  | i >= length xs = Nothing
  | otherwise      = Just (xs !! i)

-- TREE FUNCTIONS

-- accessor which returns the tree label
getNode :: Tree a -> a
getNode (Node x _) = x

-- accessor which returns the tree forest
getForest :: Tree a -> Forest a
getForest (Node _ xs) = xs

-- accessor which might return a forest
fromTree' :: Tree a -> Maybe (Forest a)
fromTree' (Node _ xs) = Just xs

-- pointfree constructor which makes a tree
leaf :: a -> Tree a
leaf = (flip $ Node) []

-- Graded class
class Graded a where
        grade :: a -> Int

instance Graded (Tree a) where
        grade = deg

-- Operad class
class Operad a where 
        degree :: a -> Int
        unit   :: a
        o      :: a -> [a] -> a

-- trees are an instance of operads
instance (Eq a) => Operad (Tree (Maybe a)) where 
        degree   = deg
        unit     = emptyLeaf 
        t `o` ts = ocompose t ts
-- generalize Maybe for monad M?
-- TODO don't like duplicated t

-- DEGREE
deg :: Tree a -> Int
deg = length . listGraftPath

-- UNIT
emptyLeaf :: Tree (Maybe a)
emptyLeaf = leaf Nothing

-- OCOMPOSITION
ocompose :: Eq a => Tree a -> Forest a -> Tree a
ocompose t ts = maybe t id (operadTreeMaybe t ts)

-- needs to graft at ends
-- graftAtPath t untuple (path, graftTree)
operadTreeMaybe :: Eq a => Tree a -> Forest a -> Maybe (Tree a)
operadTreeMaybe t ts = foldl (uncurry . graftAtPath) (Just t) (zip ts (listGraftPathD t))


-- takes a path and a set of immediate branch paths and appends them
-- appendPath :: Path -> Path -> [Path]
-- appendPath path branches = mapM (:) branches path
-- appendPath [1] [2] 

-- pointless version
appendPath' :: Path -> Path -> [Path]
appendPath' = flip $ mapM (:)
-- appendPath' [1] [2]

-- assigns a tree to a tuple of the new node and the path it took to get there.
-- the path is backwards because it concatenates from the left, so we reverse
-- the path at the null case of `getLeafPath`
zipTreePath :: Forest a -> Path -> [(Tree a, Path)]
zipTreePath subtrees path = zip subtrees (appendPath' path (enum subtrees))

-- calculates the degree of the tree
degT :: Tree a -> Int
degT t = length $ listGraftPathD t

-- prints the degree of the tree, ignoring null nodes
degT' :: Tree a -> Int
degT' t = length $ (filter (not . null)) (listGraftPathD t)

-- lists the possible places   
listLeafPath :: (Tree a, Path) -> [Path]
listLeafPath ((Node _ []), path)       = [reverse path]
listLeafPath ((Node _ subtrees), path) = do
  (zipTreePath subtrees path) >>= listLeafPath
-- EXAMPLES
-- listLeafPath (t, []) = [[]]
-- listLeafPath (t1, []) = [[1]]

tup :: Tree a -> (Tree a, Path)
tup t = (t, [0])

-- lists the paths of places to graft
listGraftPath :: Tree a -> [Path]
-- listGraftPath = tail <$> listLeafPath . tup
listGraftPath t = tail <$> listLeafPath (t, [0])
-- listGraftPath _t1

-- lists the paths of the tree which are diminished
listGraftPathD :: Tree a -> [Path]
listGraftPathD t = diminishEach <$> (listGraftPath t)

-- | borrowed from StringTemplate
-- takes a value, a monadic value, and returns either  
(|=) :: (Monad m) => a -> m a1 -> m (a, a1)
k |= v = return . (,) k =<< v
infixl 5 |=

-- this function gets the node of a tree given a path
-- m a -> p -> m (a,p)
goToNodeMaybe :: Tree a -> Path -> Maybe (Path, Tree a)
goToNodeMaybe t []   = Just ([], t)
goToNodeMaybe t path = do
  tuple <- (tail path) |= indexMaybe (fromTree' t) (head path)
  uncurry goToNodeMaybe (snd tuple, fst tuple)

--
-- 
graftFurther :: Eq a => Forest a -> Tree a -> Path -> Tree a -> Maybe (Tree a)
graftFurther forest t path tree
  | forest!!crumb==tree  = graftOnPath (Just (forest!!crumb)) t (tail path)
  | otherwise            = Just (tree)
  where crumb = head path

-- graft a tree t onto the leaf of s specified by a path
-- if t has no leaves, it should just update the leaf
graftOnPath :: Eq a => Maybe (Tree a) -> Tree a -> Path -> Maybe (Tree a)
graftOnPath Nothing _ _  = Nothing
graftOnPath (Just (Node a forest)) t path
  | (goToNodeMaybe (Node a forest) path) == Nothing = Nothing
  | path == []                                      = Just (Node a [t])
  | otherwise                                       = Just (Node a (body forest t path))
  where
    body _forest _t _path = fromJust $ sequence (graftFurther _forest _t _path <$> _forest)

-- this allows for unit
-- for example: t2 has path [0,3,1] we want to graft t1 to. 
-- graftAtPath (Just t2) t1 [2,0]
graftAtPath :: Eq a => Maybe (Tree a) -> Tree a -> Path -> Maybe (Tree a)
graftAtPath Nothing _ _ = Nothing
graftAtPath (Just (Node a forest)) t path
  | (goToNodeMaybe (Node a forest) path) == Nothing = Nothing
  | path                                 == []      = Just (Node a [t])
  | degT' t                              == 0       = Just (Node a forest)
  | otherwise                                       = Just (Node a (body forest t path))
  where
    body _forest _t _path = fromJust $ sequence (graftAtFurther _forest _t _path <$> _forest)

--
--
graftAtFurther :: Eq a => Forest a -> Tree a -> Path -> Tree a -> Maybe (Tree a)
graftAtFurther forest t path tree
  | forest!!crumb==tree  = graftAtPath (Just (forest!!crumb)) t (tail path)
  | otherwise            = Just (tree)
  where crumb = head path

-- TODO uncurry?
graftOnPathTuple :: Eq a => Maybe (Tree a) -> (Tree a, Path) -> Maybe (Tree a)
graftOnPathTuple u (t, p) = graftOnPath u t p

-- 
-- F(t)(y(x))=f^(m)(y(x))(F(t_1)(y(x))...F(t_m)(y(x)))
-- returns a list of path lists
-- ex., printLeafPaths t2 
-- [0,1]
-- [0,2,1]
printLeafPaths :: Tree a -> IO ()
printLeafPaths t = sequence_ $ getLeafPath (t, [0]) -- [0] is the root

-- IO
getLeafPath :: (Tree a, Path) -> [IO ()]
getLeafPath ((Node _ []), path)        = [print $ reverse path]
getLeafPath ((Node _ subtrees), path) = do
  (zipTreePath subtrees path) >>= getLeafPath

tree_apply :: (Num a) => [a] -> Tree a
tree_apply xs = Node 1 [leaf x | x <- xs]

corolla :: Int -> Tree Int
corolla n = Node 1 [leaf x | x <- [1..n]]
-- we want an apply method (Node (x -> x+1) [])(2) = Node 3 []

someFunc :: IO ()
someFunc = putStrLn "someFunc"


_t1 = Node (Just 1) []
-- _t2 = Node (Just 2) [(Node (Just 2) [])]
-- _t11 = Node 1 [_t1]

-- _t1o = _t1 `o` [_t1]
_tn = Node 1 [Node 2 [Node 3 []]
             ,Node 3 [Node 3 [Node 4 []
                             ,Node 5 []
						                 ,Node 6 [Node 7 []]]]
				     ,Node 4 []]
