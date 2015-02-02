----------------------------------------------------------------
--Exercise 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' =  sum . filter (even) . takeWhile (/=1) . iterate (\n -> ( if even n then n `div` 2 else (3 * n + 1)))


----------------------------------------------------------------
--Exercise 2
data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = snd . foldr addNode (2, Leaf)

addNode :: a -> (Integer, Tree a) -> (Integer, Tree a) 
addNode v (d, Leaf) = (d, Node 0 Leaf v Leaf)
addNode v (d, Node i Leaf v' Leaf) = ((d + 1), Node 1 (Node 0 Leaf v Leaf) v' Leaf)
addNode v (d, Node i ln v' Leaf) = (d, Node d ln v' (Node 0 Leaf v Leaf))
addNode v (d, Node i ln@(Node j _ _ _) v' rn@(Node k _ _ _)) | j <= k = (d,  Node i (getNode $ addNode v (d-1,ln)) v' rn)
                                                             | otherwise = (d, Node i ln v' (getNode $ addNode v (d-1,rn)))
  where
    getNode (d, n) = n



-- A not so elegent attempt at a pretty printer for the above Tree; still needs work

prettyRootNode' d (Node v l a r) = "Node " ++ (show v) ++ prettyNode 2 l ++ prettyVal 2 a ++ prettyNode 2 r

prettyNode d (Node 1 l a r) = spaces d ++ "(Node " ++ (show 1) ++ prettyNode 0 l ++ " " ++ show a ++ prettyNode 0 r
prettyNode d (Node 0 l a r) = spaces d ++ " (Node " ++ (show 0) ++ prettyNode d l ++ show a ++ prettyNode d r ++ ")"
prettyNode d (Node v l a r) = "\n" ++ spaces d ++ "(Node " ++ show v ++ "\n" ++ prettyNode (2 + d) l ++ prettyVal (2+d) a ++ "\n" ++ prettyNode (2 + d) r
prettyNode d Leaf = " Leaf"

prettyVal d v = "\n" ++ spaces d ++ (show v)

spaces sp = concat $ replicate sp " "

addReturn nv | nv < 1 = ""
             | otherwise = "\n"


----------------------------------------------------------------
--Exercise 3

xor :: [Bool] -> Bool
xor = foldr (\a b -> if a == True then not b else b) False


map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x b -> f x : b) []
