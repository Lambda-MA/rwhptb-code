module Ch08 where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
    where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
isElfFile path = do
  content <- L.readFile path
  return (hasElfMagic content)
  
closing = readPrice . (!!4) . L8.split ','

readPrice :: L8.ByteString -> Maybe Int
readPrice str =
    case L8.readInt str of
      Nothing             -> Nothing
      Just (dollars,rest) ->
        case L8.readInt (L8.tail rest) of
          Nothing           -> Nothing
          Just (cents,more) ->
            Just (dollars * 100 + cents)
    
highestClose :: L8.ByteString -> Maybe Int
highestClose = maximum . (Nothing:) . map closing . L8.lines

highestCloseFrom path = do
    contents <- L8.readFile path
    print (highestClose contents)


f = do
    s <- L8.readFile "prices.csv"
    let s2 = L8.lines s
    print $ closing $ head $tail s2

f2 = L8.readFile "prices.csv" >>= (\s1 -> print $ L8.lines s1)