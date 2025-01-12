module Learn where

import Data.List

data Form = P Integer | Neg Form | Disj Form Form | Conj Form Form
    deriving (Eq,Ord)

instance Show Form where
    show (P 1) = "p"
    show (P 2) = "q"
    show (P 3) = "r"
    show (P 4) = "s"
    show (P 5) = "t"
    show (P n) = "p"++ show n
    show (Neg p) = "~"++ show p
    show (Disj p q) = "("++ show p ++ "|" ++ show q ++")"
    show (Conj p q) = "(" ++ show p ++ "&" ++ show q ++ ")"
 




-- in semantic tableaux, the left leaf represents
-- formulae that are true, while the right leaf 
-- represents formulae that are false

data BinaryTree a = Leaf a
                  | Node a (BinaryTree a) (BinaryTree a)
    deriving (Show, Eq)

newtype LTree = BinaryTree ([Form], [Form])
{--
In semantic tableaux, each leaf contains a pair
of lists of formulae ([Form], [Form]), where the first
list is a list of true formulae, while the second -- 
list of false formulae21    
--}


semTabl :: ([Form], [Form]) -> BinaryTree ([Form], [Form])
semTabl ([], []) = Leaf ([], [])
semTabl ((P n):xs, ys) =  addToLeaves (P n) (semTabl (xs, ys))
semTabl ((Neg p):xs,ys) = semTabl (xs, p:ys)
semTabl ((Conj p q):xs,ys) = semTabl (p:q:xs, ys)
semTabl((Disj p q):xs, ys) = Node ((Disj p q):xs, ys) (semTabl (p:xs, ys)) (semTabl (q:xs, ys))
semTabl (xs, (P n):ys) =   addNegToLeaves (P n) (semTabl (xs, ys))
semTabl (xs, (Neg p):ys) = semTabl (p:xs, ys)
semTabl (xs, (Conj p q):ys) = semTabl ((Disj (Neg p) (Neg q)):xs, ys)
semTabl (xs, (Disj p q):ys) = semTabl ((Conj (Neg p) (Neg q)):xs, ys)

                     

addNegToLeaves :: Form -> BinaryTree ([Form], [Form]) -> BinaryTree ([Form], [Form])
addNegToLeaves p (Leaf (xs,ys)) = Leaf (xs, p:ys)
addNegToLeaves p (Node (xs, ys) a b) = Node (xs, p:ys) (addNegToLeaves p a) (addNegToLeaves p b)

addToLeaves :: Form -> BinaryTree ([Form], [Form]) -> BinaryTree ([Form], [Form])
addToLeaves p (Leaf (xs,ys)) = Leaf(p:xs, ys)
addToLeaves p (Node (xs, ys) a b) = Node (p:xs, ys) (addToLeaves p a) (addToLeaves p b)

                                            

getFinalLeaves :: BinaryTree ([Form], [Form]) -> [([Form], [Form])]
getFinalLeaves (Leaf (xs, ys)) = [(xs, ys)]
getFinalLeaves (Node (xs, ys) a b) = (getFinalLeaves a) ++ (getFinalLeaves b)

intList :: Eq a => [a] -> [a] -> [a]
intList xs ys = [ x | x <- xs++ys, (x `elem` xs) && (x `elem` ys)]
-- intersection operation on lists

isLeavesSat :: Form -> [Bool]
isLeavesSat p =   map (\x -> null $ (fst x) `intList` (snd x))  finals where
    finals = (getFinalLeaves . semTabl) ([p], [])

isSat :: Form -> Bool
isSat p = or (isLeavesSat p) 

taut :: Form
taut = Disj (Neg (P 1)) (P 1)

testForm :: Form -> BinaryTree ([Form], [Form])
testForm p =semTabl ([p], [])




type Assign = Integer -> Bool

powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = [x:ps | ps <- powerSet xs] ++ powerSet xs

listToAssignment :: [Integer] -> Assign
listToAssignment [] n = False
listToAssignment (x:xs) n | x==n = True
                          | otherwise = listToAssignment xs n


allAsigns :: [Integer] -> [Integer -> Bool]
allAsigns xs = [\x -> elem x ps | ps <- powerSet xs]


sat :: Assign -> Form -> Bool
sat f (P n) = f n
sat f (Neg p) = not $ sat f p
sat f (Disj p q) = sat f p || sat f q
sat f (Conj p q) = sat f p && sat f q


allVar :: Form -> [Integer]
allVar (P n) = [n]
allVar (Neg p) = allVar p
allVar (Disj p q) = allVar p ++ allVar q 
allVar (Conj p q) = allVar p ++ allVar q

isValid :: Form -> Bool
isValid p = all (\x -> sat x p) $ allAsigns $ allVar p

