module Data.Hamt (empty, insert, find, hamt, (Data.Hamt.!), Hamt(..), findWithDefault) where
import Data.Hamt.Array
import Data.Hamt.Bits
import Data.Hamt.List
import Data.Hamt.Types
import Data.List (lookup)
import Data.Array.Diff
import Data.Array.IArray
import Data.Hashable
import Data.Word 

empty :: Hamt a b
empty = Empty

wordHash :: Hashable a => a -> Word
wordHash key = fromIntegral $ hash key

insertWithMask :: (Hashable a, Eq a) => Hamt a b -> a -> Word -> b -> Int -> Hamt a b
insertWithMask Empty key _ value _ = {-# SCC "InsertKeyValue" #-} KeyValue key value
insertWithMask (KeyValue oldkey oldvalue) newkey hashvalue newvalue bitseries = 
    if newkey == oldkey then
        {-# SCC "InsertKeyValue" #-} KeyValue newkey newvalue
    else if hashvalue == (wordHash oldkey) then
             -- There was a collision.  Create a bucket to store these in.
             {-# SCC "InsertKeyValueBucket" #-} KeyValueBucket hashvalue [(oldkey, oldvalue), (newkey, newvalue)]
    else
        let oldsubkey = hashBits oldkey bitseries
        -- Convert this node into an equivalent TrieMap and try again.  This requires
        -- an extra copy, but elegantly handles the case where the two values have the
        -- same subkey.
        in {-# SCC "AllocateNewTrieMap" #-} insertWithMask
                    (TrieMap (newArrayWith [(oldsubkey, KeyValue oldkey oldvalue)]))
                    newkey hashvalue newvalue bitseries
insertWithMask (KeyValueBucket buckethashvalue assoclist) key hashvalue value bitseries =
    if hashvalue == buckethashvalue then
        {-# SCC "UpdatingKeyValueBucket" #-}KeyValueBucket buckethashvalue (update key value assoclist)
    else
        let oldsubkey = getSubkey buckethashvalue bitseries
        in {-# SCC "AllocateNewTrieMap" #-} insertWithMask
           (TrieMap (newArrayWith [(oldsubkey, KeyValueBucket buckethashvalue assoclist)]))
           key hashvalue value bitseries
insertWithMask (TrieMap arr) key hashvalue value bitseries =
    let subkey = getSubkey hashvalue bitseries
    in {-# SCC "UpdateTrieMap" #-} TrieMap (arr // [(subkey, insertWithMask 
                                   ((Data.Array.IArray.!) arr subkey) 
                                   key 
                                   hashvalue 
                                   value 
                                   (bitseries + 1))])
                 
findWithMask :: (Hashable a, Eq a) => Hamt a b -> a -> Word -> Int -> Maybe b
findWithMask Empty _ _ _ =  {-# SCC "FindOnEmpty" #-} Nothing
findWithMask (KeyValue k v) key _ _ = {-# SCC "FindOnKeyValue" #-} if k == key then
                                        Just v
                                    else
                                        Nothing
findWithMask (TrieMap arr) key hashvalue bitseries = 
    let subkey = getSubkey (fromIntegral hashvalue) bitseries
    in {-# SCC "FindOnTrieMap" #-} findWithMask ((Data.Array.IArray.!) arr subkey) key hashvalue (bitseries+1)
findWithMask (KeyValueBucket _ assoclist) key hashvalue bitseries =
    {-# SCC "FindOnKeyValueBucket" #-} lookup key assoclist

-- | Find a value in the hash trie
find :: (Hashable a, Eq a) 
        => Hamt a b -- ^ The hash trie to search
        -> a        -- ^ The key
        -> Maybe b  -- ^ The value (if it exists)
find tn key = {-# SCC "Find" #-} findWithMask tn key (fromIntegral $ hash key) 1 

-- | Find a value in the hash trie returning a given default if it is not found.
findWithDefault :: (Hashable a, Eq a) 
                   => Hamt a b -- ^ The hash trie
                   -> a        -- ^ The key to search for
                   -> b        -- ^ The default value
                   -> b        -- ^ The found value
findWithDefault trie key def = case find trie key of
                                 Just val -> val
                                 Nothing -> def

(!) :: (Hashable a, Eq a, Show b) => Hamt a b -> a -> Maybe b
(!) tn key = find tn key

-- | Insert a value into the hash trie
insert :: (Eq a, Hashable a) 
          => Hamt a b -- ^ The hash trie to insert into
          -> a        -- ^ The key to insert this under
          -> b        -- ^ The value to be inserted
          -> Hamt a b -- ^ The resulting hash trie
insert tn key value = {-# SCC "Insert" #-} insertWithMask tn key (fromIntegral $ hash key) value 1

insertPair :: (Eq a, Hashable a) => Hamt a b -> (a, b) -> Hamt a b
insertPair tn (key, value) = insert tn key value

insertPairs :: (Eq a, Hashable a) => Hamt a b -> [(a, b)] -> Hamt a b
insertPairs tn pairs = foldl insertPair tn pairs

-- | Create a hash trie, built from a list of 
hamt :: (Eq a, Hashable a) => [(a, b)] -> Hamt a b
hamt [] = Empty
hamt pairs = insertPairs Empty pairs
