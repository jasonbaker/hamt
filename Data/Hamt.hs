module Data.Hamt (empty,
                  insert,
                  find,
                  hamt,
                  (Data.Hamt.!),
                  Hamt(..),
                  HashTrie(..),
                  findWithDefault, 
                  delete) where
import Data.Hamt.Array
import Data.Hamt.Bits
import Data.Hamt.KeyInfo
import Data.Hamt.List
import Data.Hamt.Types
import Data.Int
import Data.List (lookup)
import Data.Array
                                 

-- | Find a value in the hash trie
find :: Eq a 
        => a        -- ^ The key
        -> HashTrie a b -- ^ The hash trie to search
        -> Maybe b  -- ^ The value (if it exists)
find key (HashTrie hf tn) = {-# SCC "Find" #-} findWithMask tn (newKeyInfo key hf)

-- | Find a value in the hash trie returning a given default if it is not found.
findWithDefault :: Eq a 
                   => a        -- ^ The key to search for
                   -> b        -- ^ The default value
                   -> HashTrie a b -- ^ The hash trie
                   -> b        -- ^ The found value
findWithDefault key def trie = case find key trie of
                                 Just val -> val
                                 Nothing -> def

(!) :: Eq a => HashTrie a b -> a -> Maybe b
(!) tn key = find key tn 

-- | Insert a value into the hash trie
insert :: Eq a
          => a        -- ^ The key to insert this under
          -> b        -- ^ The value to be inserted
          -> HashTrie a b -- ^ The hash trie to insert into
          -> HashTrie a b -- ^ The resulting hash trie
insert key value (HashTrie hashfunc tn) = {-# SCC "Insert" #-}
                                          let keyinfo = newKeyInfo key hashfunc 
                                          in HashTrie hashfunc (insertWithMask tn keyinfo value)

-- | Delete a key from the hash trie.  Returns the same trie if the key does
-- does not exist.
delete :: Eq a => a -> HashTrie a b -> HashTrie a b
delete key (HashTrie hashfunc tn) = HashTrie hashfunc (deleteWithMask tn (newKeyInfo key hashfunc))

-- | Create a hash trie, built from an association list.
hamt :: Eq a  => [(a, b)] ->  (a -> Int32) -> HashTrie a b
hamt [] hashfunc = HashTrie hashfunc Empty
hamt pairs hashfunc = insertPairs pairs (HashTrie hashfunc Empty)

-- | An empty hash trie.
empty :: Eq a => (a->Int32) -> HashTrie a b
empty hashfunc = HashTrie hashfunc Empty

insertWithMask :: Eq a => Hamt a b -> KeyInfo a -> b -> Hamt a b
insertWithMask Empty keyinfo value = {-# SCC "InsertKeyValue" #-} KeyValue (kfKey keyinfo) value
insertWithMask (KeyValue oldkey oldvalue) keyinfo newvalue = 
    let newkey = kfKey keyinfo
        hashvalue = kfHash keyinfo
        hashfunc = kfHashfunc keyinfo
        bitseries = kfBitseries keyinfo
        oldhash = hashfunc oldkey
    in if newkey == oldkey then
        KeyValue newkey newvalue
    else if hashvalue == oldhash then
             -- There was a collision.  Create a bucket to store these in.
             KeyValueBucket hashvalue [(oldkey, oldvalue), (newkey, newvalue)]
    else
        let oldsubkey = getSubkey oldhash bitseries
        -- Convert this node into an equivalent TrieMap and try again.  This requires
        -- an extra copy, but elegantly handles the case where the two values have the
        -- same subkey.  Somewhat naive, but elegant and effective.
        in {-# SCC "AllocateNewTrieMap" #-} insertWithMask
                    (TrieMap (newArrayWith [(oldsubkey, KeyValue oldkey oldvalue)]))
                    keyinfo newvalue 
insertWithMask (KeyValueBucket buckethashvalue assoclist) keyinfo value =
    let hashvalue = kfHash keyinfo
        key = kfKey keyinfo
        bitseries = kfBitseries keyinfo
    in if hashvalue == buckethashvalue then
        {-# SCC "UpdatingKeyValueBucket" #-}KeyValueBucket buckethashvalue (update key value assoclist)
    else
        let oldsubkey = getSubkey buckethashvalue bitseries
        -- Again, somewhat naive, but elegant and effective.
        in insertWithMask
           (TrieMap (newArrayWith [(oldsubkey, KeyValueBucket buckethashvalue assoclist)]))
           keyinfo value 
insertWithMask (TrieMap arr) keyinfo value =
    let hashvalue = kfHash keyinfo
        bitseries = kfBitseries keyinfo
        subkey = getSubkey hashvalue bitseries
    in {-# SCC "UpdateTrieMap" #-} TrieMap (arr // [(subkey, insertWithMask 
                                   ((Data.Array.!) arr subkey) 
                                   (incBitseries keyinfo)
                                   value)])
                 
findWithMask :: Eq a => Hamt a b -> KeyInfo a -> Maybe b
findWithMask Empty _ =  {-# SCC "FindOnEmpty" #-} Nothing
findWithMask (KeyValue k v) keyinfo = {-# SCC "FindOnKeyValue" #-} 
                                      let key = kfKey keyinfo
                                      in if k == key then
                                             Just v
                                         else
                                             Nothing
findWithMask (TrieMap arr) keyinfo  = 
    let subkey = getSubkey (kfHash keyinfo) (kfBitseries keyinfo)
    in {-# SCC "FindOnTrieMap" #-} findWithMask ((Data.Array.!) arr subkey) (incBitseries keyinfo)
findWithMask (KeyValueBucket _ assoclist) keyinfo =
    {-# SCC "FindOnKeyValueBucket" #-} lookup (kfKey keyinfo) assoclist

insertPair :: Eq a => HashTrie a b -> (a, b) -> HashTrie a b
insertPair tn (key, value) = insert key value tn 

insertPairs :: Eq a => [(a, b)] -> HashTrie a b-> HashTrie a b
insertPairs pairs tn = foldl insertPair tn pairs 

deleteWithMask :: Eq a => Hamt a b -> KeyInfo a -> Hamt a b
deleteWithMask Empty _ = Empty
deleteWithMask (KeyValue key value) keyinfo = if (kfKey keyinfo) == key then
                                       Empty
                                   else
                                       KeyValue key value
deleteWithMask (KeyValueBucket subkey pairs) keyinfo = 
    -- Perhaps a minor issue, but if this leaves the KeyValueBucket empty, we're
    -- still leaving the KeyValueBucket here instead of returning Empty.
    let delkey = kfKey keyinfo
    in KeyValueBucket subkey (filter (\(key, value) -> key /= delkey) pairs)
deleteWithMask (TrieMap arr) keyinfo =
    -- Again, possibly minor issue, but if this leaves an empty TrieMap, return
    -- it instead of Empty.
    let hashvalue = kfHash keyinfo
        bitseries = kfBitseries keyinfo
        subkey = getSubkey hashvalue bitseries in
    TrieMap (arr // [(subkey, deleteWithMask ((Data.Array.!) arr subkey) (incBitseries keyinfo))])



