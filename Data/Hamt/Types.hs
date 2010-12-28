module Data.Hamt.Types where
import Data.Array.Diff
import Data.Word

data Hamt a b = KeyValue a b
              | KeyValueBucket Word [(a, b)]
              | TrieMap (DiffArray Word (Hamt a b)) 
              | Empty
                deriving (Show)

