

{-# LANGUAGE TemplateHaskell #-}


import Test.QuickCheck.All

import Test.QuickCheck
import Data.List
import Control.Monad
import Prelude hiding ((<>), empty)
import Test.QuickCheck.All


return []
runTests = $quickCheckAll

main :: IO ()
main = runTests >>= \passed -> if passed then putStrLn "All tests passed."
                                         else putStrLn "Some tests failed."
