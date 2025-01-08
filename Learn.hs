module Learn where

import Data.List

data Form = P Integer | Neg Form | Disj Form Form | Conj Form Form
    deriving (Eq,Ord,Show)

data TreeForm =  Leaf [Form] [Form] | Nodes TreeForm TreeForm
    deriving (Eq,Ord, Show)

-- in semantic tableaux, the left leaf represents
-- formulae that are true, while the right leaf 
-- represents formulae that are false

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



getFinalLeaves :: TreeForm -> [([Form], [Form])]
getFinalLeaves (Leaf xs ys) = [(xs, ys)]
getFinalLeaves (Nodes x y) = (getFinalLeaves x) ++ (getFinalLeaves y)

addTrees :: TreeForm -> TreeForm -> TreeForm
addTrees (Leaf as bs) (Leaf xs ys) = Leaf (as++xs) (bs++ys)
addTrees (Nodes  a b) (Nodes x y) = Nodes (addTrees a x) (addTrees b y)

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

isSat :: Form -> Bool
isSat p =  foldr (&&) True (map (\x -> [] == (fst x) `intList` (snd x))  finals) where
    finals = (getFinalLeaves . semTabl) p


sat :: Assign -> Form -> Bool
sat f (P n) = f n
sat f (Neg p) = not $ sat f p
sat f (Disj p q) = sat f p || sat f q
sat f (Conj p q) = sat f p && sat f q


allVar :: Form -> [Integer]
allVar (P n) = [n]
allVar (Neg p) = allVar p
allVar (Disj p q) = allVar p ++ allVar q 

isValid :: Form -> Bool
isValid p = all (\x -> sat x p) $ allAsigns $ allVar p

