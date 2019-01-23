module Ch11 where

import Test.QuickCheck
import Data.List
import Control.Monad
import Prelude hiding ((<>))

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

-- funções para o Documento

empty :: Doc
empty = Empty

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line
{-

-- versão mais direta

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
--}

-- versao monadica"
instance Arbitrary Doc where
    arbitrary =
        oneof [ return Empty
              , liftM  Char   arbitrary
              , liftM  Text   arbitrary
              , return Line
              , liftM2 Concat arbitrary arbitrary
              , liftM2 Union  arbitrary arbitrary ]


-- testes
prop_empty_id x =
    empty <> x == x
  &&
    x <> empty == x

prop_char c   = char c   == Char c
prop_text s   = text s   == if null s then Empty else Text s
prop_line     = line     == Line
prop_double d = double d == text (show d)