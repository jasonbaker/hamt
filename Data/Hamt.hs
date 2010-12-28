{-# OPTIONS_GHC -XGADTs #-}
module Data.Hamt where
import Data.Hamt.Array
import Data.Hamt.Bits
import Data.Hamt.Debug
import Data.Hamt.Types
import Data.Array
import Data.Hashable

empty :: Hamt a b
empty = Empty

insertWithMask :: (Hashable a, Eq a) => Hamt a b -> a -> Int -> b -> Int -> Hamt a b
insertWithMask Empty key _ value _ = KeyValue key value
insertWithMask (KeyValue oldkey oldvalue) newkey hashvalue newvalue bitseries = 
    let newsubkey = getSubkey (fromIntegral hashvalue) bitseries
        oldsubkey = hashBits oldkey bitseries
    in TrieMap (newArrayWith [(newsubkey, KeyValue newkey newvalue),
                              (oldsubkey, KeyValue oldkey oldvalue)])
                 
findWithMask :: (Hashable a, Eq a) => Hamt a b -> a -> Int -> Int -> Maybe b
findWithMask Empty _ _ _= Nothing
findWithMask (KeyValue k v) key _ _ = if k == key then
                                        Just v
                                    else
                                        Nothing
findWithMask (TrieMap array) key hashvalue bitseries = 
    let subkey = getSubkey (fromIntegral hashvalue) bitseries
    in findWithMask ((Data.Array.!) array subkey) key hashvalue (bitseries+1)

find :: (Hashable a, Eq a) => Hamt a b -> a -> Maybe b
find tn key = findWithMask tn key (hash key) 1 

(!) :: (Hashable a, Eq a) => Hamt a b -> a -> Maybe b
(!) tn key = find tn key

insert :: (Eq a, Hashable a) => Hamt a b -> a -> b -> Hamt a b 
insert tn key value = insertWithMask tn key (hash key) value 1

insertPair :: (Eq a, Hashable a) => Hamt a b -> (a, b) -> Hamt a b
insertPair tn (key, value) = insert tn key value

insertPairs :: (Eq a, Hashable a) => Hamt a b -> [(a, b)] -> Hamt a b
insertPairs tn pairs = foldl insertPair tn pairs

hamt :: (Eq a, Hashable a) => [(a, b)] -> Hamt a b
hamt [] = Empty
hamt pairs = insertPairs Empty pairs
