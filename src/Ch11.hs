module Ch11 where

import Test.QuickCheck
import Data.List

-------------------------------------------------------------------------
-- um pequeno exemplo para experimentar a geração de dados randomicos
-------------------------------------------------------------------------
data Ternary
    = Yes
    | No
    | Unknown
    deriving (Eq,Show)

instance Arbitrary Ternary where
    --arbitrary     = elements [Yes, No, Unknown]
    arbitrary     = do
        n <- choose (0, 2) :: Gen Int
        return $ case n of
                      0 -> Yes
                      1 -> No
                      _ -> Unknown

----------------------------------------------------------------------

-- Defined in ‘Test.QuickCheck.Arbitrary’ 
--instance Arbitrary Char where
    --arbitrary = elements (['A'..'Z'] ++ ['a' .. 'z'] ++ " ~!@#$%^&*()")


data Doc = Empty
    | Char Char
    | Text String
    | Line
    | Concat Doc Doc
    | Union Doc Doc
    deriving (Show,Eq)

instance Arbitrary Doc where
        arbitrary = do
            n <- choose (1,6) :: Gen Int
            case n of
                 1 -> return Empty
    
                 2 -> do x <- arbitrary
                         return (Char x)
    
                 3 -> do x <- arbitrary
                         return (Text x)
    
                 4 -> return Line
    
                 5 -> do x <- arbitrary
                         y <- arbitrary
                         return (Concat x y)
    
                 6 -> do x <- arbitrary
                         y <- arbitrary
                         return (Union x y)