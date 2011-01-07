module Main where
import Test.HUnit
import Data.Hamt
import Data.Hamt.Types

test_insert = TestCase (assertEqual "Assert that insert changes something"
                                   (find "asdf" ((hamt [("asdf", 1)])::Hamt String Int)) (Just 1))

test_empty_find = TestCase (assertEqual "Assert empty trie returns Nothing"
                                        (find "asdf" (empty::Hamt String Int)) Nothing)

test_key_value_find_just = TestCase (assertEqual
                                     "Assert that a key is found in a KeyValue node"
                                     (find "asdf" (KeyValue "asdf" 1)) (Just 1))

test_key_value_find_nothing = TestCase (assertEqual
                                        "Assert that a key is not found in a KeyValue node"
                                        (find "jkl;" (KeyValue "asdf" 1)) Nothing)

test_find_in_single_level_trie = TestCase(assertEqual
                                          "Find a key ('asdf') in a single level trie"
                                          (find "asdf" (hamt [("asdf", 1), ("jkl;", 2)])) (Just 1))

test_find_other_in_single_level_trie = TestCase(assertEqual
                                          "Find a key ('jkl;') in a single level trie"
                                          (find "jkl;" (hamt [("asdf", 1), ("jkl;", 2)])) (Just 2))

test_findWithDefault = TestCase(assertEqual
                                "Ensure findWithDefault properly returns the default"
                                (findWithDefault "jkl;" 2 (hamt [("asdf", 1)])) 2)

test_del_empty = TestCase(assertEqual
                          "Delete a key from an empty node"
                          ((delete "asdf" empty)::Hamt String Int) (Empty::Hamt String Int))

test_del_kv = TestCase(assertEqual
                       "Delete a key from a single-key node"
                       (delete "asdf" (hamt [("asdf", 1)])) Empty)

test_del_triemap = TestCase(assertEqual
                            "Delete a key from an array node"
                            (find "asdf" (delete "asdf" (hamt [("asdf", 1), ("jkl;", 2)]))) Nothing)

tests = TestList [test_insert, test_empty_find, test_key_value_find_nothing,
                 test_find_in_single_level_trie, test_find_other_in_single_level_trie,
                 test_del_empty, test_del_kv, test_del_triemap, test_findWithDefault]

main = runTestTT tests

