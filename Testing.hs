module Testing where

import Parser
import Data.List
import Test.QuickCheck
import Learn


instance Arbitrary Form where
    arbitrary = sized randomForm where
        randomForm :: Int -> Gen Form
        randomForm 0 = P <$> elements [1,2]
        randomForm n = oneof
            [ P <$> elements [1,2]
            , Neg <$> randomForm (n `div` 2)
            , Conj <$> randomForm (n `div` 2) <*> randomForm (n `div` 2) 
            , Disj <$> randomForm (n `div` 2) <*> randomForm (n `div` 2) ]


testTabl = quickCheck (\x -> isSat (Neg x)==(not . isValid) x)
testParser = quickCheck (\x -> (parsing . show) x==Just x)






