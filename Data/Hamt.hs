{-# OPTIONS_GHC -XGADTs #-}
module Data.Hamt where
import Data.Bits
import Data.Array
import Data.Hashable

data Hamt a b = KeyValue a b
                  | TrieMap (Array Int (Hamt a b))
                  | Empty
                    deriving (Show, Eq)

popCount :: Int -> Int
popCount x =
    let x1 = x - ((x `shiftR` 1) .&. 0x55555555)
        x2 = ((x1 `shiftR` 2) .&. 0x33333333) + (x1 .&. 0x33333333)
        x3 = ((x2 `shiftR` 4) + x2) .&. 0x0f0f0f0f
        x4 = x3 + (x3 `shiftR` 8)
        x5 = x4 + (x4 `shiftR` 16)
    in x5 .&. 0x0000003f

newRoot :: Hashable a => Hamt a b
newRoot = TrieMap (array (0, 31) [(i, Empty) | i <- [0..31]])

empty :: Hashable a => Hamt a b
empty = Empty

hasKey :: (Eq a) => Hamt a b -> a -> Bool
hasKey Empty _ = False
hasKey (KeyValue k _) key = k == key
hasKey (TrieMap arr) key = False

insert :: Hashable a => Hamt a b -> a -> b -> Hamt a b 
insert tn key value = tn

insertPair :: Hashable a => Hamt a b -> (a, b) -> Hamt a b
insertPair tn (key, value) = insert tn key value

insertPairs :: Hashable a => Hamt a b -> [(a, b)] -> Hamt a b
insertPairs tn pairs = foldl insertPair tn pairs

hamt :: Hashable a => [(a, b)] -> Hamt a b
hamt [] = Empty
hamt pairs = insertPairs newRoot pairs
