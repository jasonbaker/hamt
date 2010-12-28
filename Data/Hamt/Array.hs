module Data.Hamt.Array where
import Control.Monad
import Control.Monad.ST
import Data.Array.Diff
import Data.Array.MArray
import Data.Array.ST
import Data.Hamt.Types

newArrayWith :: (Integral i, Ix i) => [(i, Hamt a b)] -> DiffArray i (Hamt a b)
newArrayWith assocs = {-# SCC "AllocateAndSetArray" #-} array (0, 31) [(i, Empty) | i <- [0..31]] // assocs

-- runST $ do
--                         arr <- array (0, 31) Empty
--                         forM_ assocs (\(idx, value) ->
--                                           writeArray arr idx value)
--                         return arr
