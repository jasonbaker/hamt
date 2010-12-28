{-# OPTIONS_GHC -XGADTs #-}
module Data.Hamt (empty, insert, find, hamt, (Data.Hamt.!), Hamt(..)) where
import Data.Hamt.Array
import Data.Hamt.Bits
import Data.Hamt.Debug
import Data.Hamt.Types
import Data.Array
import Data.Hashable
import Data.Word 
import Debug.Trace

empty :: Hamt a b
empty = Empty

insertWithMask :: (Hashable a, Eq a) => Hamt a b -> a -> Word -> b -> Int -> Hamt a b
insertWithMask Empty key _ value _ = KeyValue key value
insertWithMask (KeyValue oldkey oldvalue) newkey hashvalue newvalue bitseries = 
    let oldsubkey = hashBits oldkey bitseries
    -- Convert this node into an equivalent TrieMap and try again.  This requires
    -- an extra copy, but elegantly handles the case where the two values have the
    -- same subkey.
    in insertWithMask
                (TrieMap (newArrayWith [(oldsubkey, KeyValue oldkey oldvalue)]))
                newkey hashvalue newvalue bitseries
insertWithMask (TrieMap arr) key hashvalue value bitseries =
    let subkey = getSubkey hashvalue bitseries
    in TrieMap (arr // [(subkey, insertWithMask 
                                   ((Data.Array.!) arr subkey) 
                                   key 
                                   hashvalue 
                                   value 
                                   (bitseries + 1))])
                 
findWithMask :: (Hashable a, Eq a, Show b) => Hamt a b -> a -> Word -> Int -> Maybe b
findWithMask Empty _ _ _ =  Nothing
findWithMask (KeyValue k v) key _ _ = if k == key then
                                        Just v
                                    else
                                        Nothing
findWithMask (TrieMap arr) key hashvalue bitseries = 
    let subkey = getSubkey (fromIntegral hashvalue) bitseries
    in findWithMask ((Data.Array.!) arr subkey) key hashvalue (bitseries+1)

-- | Find a value in the hash trie
find :: (Hashable a, Eq a, Show b) 
        => Hamt a b -- ^ The hash trie to search
        -> a        -- ^ The key
        -> Maybe b  -- ^ The value (if it exists)
find tn key = findWithMask tn key (fromIntegral $ hash key) 1 

(!) :: (Hashable a, Eq a, Show b) => Hamt a b -> a -> Maybe b
(!) tn key = find tn key

-- | Insert a value into the hash trie
insert :: (Eq a, Hashable a) 
          => Hamt a b -- ^ The hash trie to insert into
          -> a        -- ^ The key to insert this under
          -> b        -- ^ The value to be inserted
          -> Hamt a b -- ^ The resulting hash trie
insert tn key value = insertWithMask tn key (fromIntegral $ hash key) value 1

insertPair :: (Eq a, Hashable a) => Hamt a b -> (a, b) -> Hamt a b
insertPair tn (key, value) = insert tn key value

insertPairs :: (Eq a, Hashable a) => Hamt a b -> [(a, b)] -> Hamt a b
insertPairs tn pairs = foldl insertPair tn pairs

-- | Create a hash trie
hamt :: (Eq a, Hashable a) => [(a, b)] -> Hamt a b
hamt [] = Empty
hamt pairs = insertPairs Empty pairs
