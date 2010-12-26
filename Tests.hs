module Main where
import Test.HUnit
import Data.Hamt

test_insert = TestCase (assertBool "Assert that insert changes something"
                               (do
                                 empty /= (insert empty "asdf" 1)))

test_empty_hasKey = TestCase (assertBool "Assert that a key is not in an empty node"
                                            (not (hasKey empty "asdf")))

test_trueKeyValue_hasKey = TestCase(assertBool "keyInValue should return True"
                                                  (hasKey (KeyValue "asdf" 1)
                                                            "asdf"))

tests = TestList [test_insert, test_empty_hasKey, test_trueKeyValue_hasKey]

main = runTestTT tests

