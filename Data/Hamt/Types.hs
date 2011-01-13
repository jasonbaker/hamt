module Data.Hamt.Types where
import Data.Array
import Data.Int

data HashTrie a b = HashTrie (a -> Int32) (Hamt a b)

instance (Show a, Show b) => Show (HashTrie a b) where
    show (HashTrie _ hamt) = "HashTrie " ++ (show hamt)

instance (Eq a, Eq b) => Eq (HashTrie a b) where
    (==) (HashTrie _ hamt1)  (HashTrie _ hamt2) = hamt1 == hamt2

data Hamt a b = KeyValue a b
              | KeyValueBucket Int32 [(a, b)]
              | TrieMap (Array Int32 (Hamt a b)) 
              | Empty
                deriving (Show, Eq)

