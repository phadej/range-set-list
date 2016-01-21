import Test.Tasty

import List
import Map
import IntMap

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [listProps, mapProps, intMapProps]
