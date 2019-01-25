{-# LANGUAGE TemplateHaskell #-}

module Ch11 where
import Test.QuickCheck.All

import Test.QuickCheck
import Data.List
import Control.Monad
import Prelude hiding ((<>))
import Test.QuickCheck.All

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

{-
O caso de uso, prettyprint, veja em: https://github.com/profsergiocosta/hs2json  

-}