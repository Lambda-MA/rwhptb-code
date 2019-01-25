
{-# LANGUAGE FlexibleContexts #-}

module Ch19.EitherMonadico where


import Control.Monad.Except

data DivByError a = DivBy0 
    | ForbiddenDenominator a 
    | OtherDivByError String 
    deriving (Eq, Read, Show)


divBy :: Integral a => a -> [a] -> Either (DivByError a) [a]
divBy = divByGeneric

divByGeneric :: (Integral a,  MonadError (DivByError a) m) =>
                 a -> [a] -> m [a]
divByGeneric _ [] = return []
divByGeneric _ (0:_) = throwError DivBy0
divByGeneric _ (10:_) = throwError (ForbiddenDenominator 10)
divByGeneric _ (20:_) = throwError (ForbiddenDenominator 20)
divByGeneric numerator (denom:xs) =
    do next <- divByGeneric numerator xs
       return ((numerator `div` denom) : next)