module Main where
import Test.HUnit
import Data.Hamt
import Data.Hamt.Types

test_insert = TestCase (assertEqual "Assert that insert changes something"
                                   (find ((hamt [("asdf", 1)])::Hamt String Int) "asdf") (Just 1))

test_empty_find = TestCase (assertEqual "Assert empty trie returns Nothing"
                                        (find (empty::Hamt String Int) "asdf") Nothing)

test_key_value_find_just = TestCase (assertEqual
                                     "Assert that a key is found in a KeyValue node"
                                     (find (KeyValue "asdf" 1) "asdf") (Just 1))

test_key_value_find_nothing = TestCase (assertEqual
                                        "Assert that a key is not found in a KeyValue node"
                                        (find (KeyValue "asdf" 1) "jkl;") Nothing)

test_find_in_single_level_trie = TestCase(assertEqual
                                          "Find a key in a single level trie"
                                          (find (hamt [("asdf", 1), ("jkl;", 2)]) "asdf") (Just 1))

tests = TestList [test_insert, test_empty_find, test_key_value_find_nothing,
                 test_find_in_single_level_trie]

main = runTestTT tests

