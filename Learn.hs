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
 


data TreeForm =  Leaf [Form] [Form] | Nodes TreeForm TreeForm
    deriving (Eq,Ord, Show)

-- in semantic tableaux, the left leaf represents
-- formulae that are true, while the right leaf 
-- represents formulae that are false





getFinalLeaves :: TreeForm -> [([Form], [Form])]
getFinalLeaves (Leaf xs ys) = [(xs, ys)]
getFinalLeaves (Nodes x y) = (getFinalLeaves x) ++ (getFinalLeaves y)

addTrees :: TreeForm -> TreeForm -> TreeForm
addTrees (Leaf as bs) (Leaf xs ys) = Leaf (as++xs) (bs++ys)
addTrees (Nodes  a b) (Nodes x y) = Nodes (Nodes (addTrees a x) (addTrees a y)) ( Nodes (addTrees b x) (addTrees b y))
addTrees (Leaf as bs) (Nodes x y) = Nodes (addTrees (Leaf as bs) x) (addTrees (Leaf as bs) y)
addTrees (Nodes x y) (Leaf as bs) = addTrees (Leaf as bs) (Nodes x y)

semTabl :: Form -> TreeForm
semTabl (P n) = Leaf [P n] []
semTabl (Neg (P n)) = Leaf [] [P n]
semTabl (Disj p q) = Nodes (semTabl p) (semTabl q)
semTabl (Conj p q) = (semTabl p) `addTrees` (semTabl q)
semTabl(Neg(Disj p q)) = (semTabl (Neg p)) `addTrees` (semTabl (Neg q)) 
semTabl(Neg(Conj p q)) = semTabl (Disj (Neg p) (Neg q))
semTabl (Neg(Neg p)) = semTabl p

taut :: Form
taut = Disj (Neg (P 1)) (P 1)


intList :: Eq a => [a] -> [a] -> [a]
intList xs ys = [ x | x <- xs++ys, (x `elem` xs) && (x `elem` ys)]
-- intersection operation on lists

isLeavesSat :: Form -> [Bool]
isLeavesSat p =   (map (\x -> [] == (fst x) `intList` (snd x))  finals) where
    finals = (getFinalLeaves . semTabl) p

isSat :: Form -> Bool
isSat p = True `elem` (isLeavesSat p)   

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

