module Data.Hamt.Types where
import Data.Array
import Data.Word

data Hamt a b = KeyValue a b
                  | TrieMap (Array Word (Hamt a b)) 
                  | Empty
                    deriving (Show)

