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


data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

addNode v (d, Leaf) = (d, Node 0 Leaf v Leaf)
addNode v (d, Node i Leaf v' Leaf) = ((d + 1), Node 1 (Node 0 Leaf v Leaf) v' Leaf)
addNode v (d, Node i ln v' Leaf) = (d, Node d ln v' (Node 0 Leaf v Leaf))
--addNode v (d, Node (d-1) ln@(Node _ _ _ _) v' Leaf)  = Node i ln v' (addNode v (d, Leaf)
addNode v (d, Node i ln@(Node j _ _ _) v' rn@(Node k _ _ _)) | j <= k = (d,  Node i (getNode $ addNode v (d-1,ln)) v' rn)
                                                             | otherwise = (d, Node i ln v' (getNode $ addNode v (d-1,rn)))
getNode (d, n) = n
--nodeDepth d n@(Node v _ _ _) = nodeDepth (Node d 

foldTree = snd . foldr addNode (2, Leaf)


prettyRootNode (Node v l a r) = "Node " ++ (show v) ++ prettyTree 0 l ++ spaces 2 ++ (show a) ++ prettyTree 0 r
--prettyTree sp (Node v Leaf a Leaf) = addReturn v ++ " (Node " ++ (show v) ++ " Leaf " ++ show a ++ " Leaf) \n"
prettyTree sp (Node v l a r) = addReturn v ++ spaces (2 + sp) ++ "(Node " ++ (show v) ++ prettyTree (sp+4) l ++ (show a) ++ prettyTree (sp+4) r
--prettyTree sp Leaf = "  Leaf \n"

spaces sp = concat $ replicate sp " "

addReturn nv | nv < 2 = ""
             | otherwise = "\n"

