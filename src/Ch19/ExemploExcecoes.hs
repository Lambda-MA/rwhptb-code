{-# LANGUAGE FlexibleContexts #-}

module Ch19.ExemploExcecoes where

import Control.Exception
import Control.Exception.Base

catchIt :: ArithException -> Maybe ()
catchIt  DivideByZero = Just ()
catchIt _ = Nothing

handler :: () -> IO ()
handler _ = putStrLn "Caught error: divide by zero"

safePrint :: Integer -> IO ()
safePrint x = handleJust catchIt handler (print x)


{--

   import Control.Exception.Base
    --import Data.Array
    
    main = toTry `catch` handler
    
    toTry = do
        print "hi"
        print (show (3 `div` 0))
        print "hi"
    
    handler :: ArithException -> IO ()
    handler DivideByZero = putStrLn "Divide by Zero!"
    handler _ = putStrLn "Some other error..."

--}