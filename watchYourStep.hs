data Tree a = Empty  Node a (Tree a ) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree = 
	Node 'P'
		(Node 'O'
			(Node 'L'
				(Node 'N' Empty Empty)
				(Node 'T' Empty Empty)
			)
			(Node 'Y'
				(Node 'S' Empty Empty)
				(Node 'A' Empty Empty)
			)
		)
		(Node 'L'
			(Node 'W'
				(Node 'C' Empty Empty)
				(Node 'R' Empty Empty)
			)
			(Node 'A'
				(Node 'A' Empty Empty)
				(Node 'C' Empty Empty)
			)
		)
		
data Direction = L | R deriving (Show)
type Directions = [Direction]

changeToP :: Directions -> Tree Char -> Tree Char
changeToP (L:ds) (Node xl r) = Node x (changeToP ds l) r
changeToP (R:ds) (Node xl r) = Node x 1 (changeToP ds r) 
changeToP [] (Node _ l r) = Node 'P' l r;


		
