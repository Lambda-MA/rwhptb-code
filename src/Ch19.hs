{-# LANGUAGE FlexibleContexts #-}

module Ch19 where

divBy :: Integral a => a -> [a] -> [a]
divBy numerator = map (numerator `div`)

divBy' :: Integral a => a -> [a] -> Maybe [a]
divBy' _ [] = Just []
divBy' _ (0:_) = Nothing
divBy' numerator (denom:xs) =
    case (divBy' numerator xs) of
      Nothing -> Nothing
      Just results -> Just ((numerator `div` denom) : results)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

divBy'' :: Integral a => a -> [a] -> [Maybe a]
divBy'' numerator denominators =
    map safeDiv denominators
    where safeDiv 0 = Nothing
          safeDiv x = Just (numerator `div` x)

divBy''' :: Integral a => a -> [a] -> Maybe [a]
divBy''' _ [] = return []
divBy''' _ (0:_) = fail "division by zero in divBy"
divBy''' numerator (denom:xs) =
    do next <- divBy''' numerator xs
       return ((numerator `div` denom) : next)

eDivBy :: Integral a => a -> [a] -> Either String [a]
eDivBy _ [] = Right []
eDivBy _ (0:_) = Left "divBy: division by 0"
eDivBy numerator (denom:xs) =
    case eDivBy numerator xs of
      Left x -> Left x
      Right results -> Right ((numerator `div` denom) : results)

--- tipos de dados para identificar os erros

data DivByError a = DivBy0
                 | ForbiddenDenominator a
                   deriving (Eq, Read, Show)

eDivBy' :: Integral a => a -> [a] -> Either (DivByError a) [a]
eDivBy' _ [] = Right []
eDivBy' _ (0:_) = Left DivBy0
eDivBy' _ (10:_) = Left (ForbiddenDenominator 10)
eDivBy' _ (20:_) = Left (ForbiddenDenominator 20)
eDivBy' numerator (denom:xs) =
    case eDivBy' numerator xs of
      Left x -> Left x
      Right results -> Right ((numerator `div` denom) : results)

-- https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/10_Error_Handling