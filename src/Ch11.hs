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

--instance Arbitrary Char where
    --arbitrary = elements (['A'..'Z'] ++ ['a' .. 'z'] ++ " ~!@#$%^&*()")