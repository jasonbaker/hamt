module Data.Hamt.Array where
import Control.Monad
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.Hamt.Types

newArrayWith :: (Integral i, Ix i) => [(i, Hamt a b)] -> Array i (Hamt a b)
newArrayWith assocs = {-# SCC "AllocateAndSetArray" #-} runSTArray $ do
                        arr <- newArray (0, 31) Empty
                        forM_ assocs (\(idx, value)->
                                          writeArray arr idx value)
                        return arr

