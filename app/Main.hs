
module Main where


import Prelude hiding ((<>))
import Ch11




main :: IO ()
main = runTests >>= \passed -> if passed then putStrLn "All tests passed."
                                         else putStrLn "Some tests failed."