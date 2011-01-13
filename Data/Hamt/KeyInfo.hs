module Data.Hamt.KeyInfo where
import Data.Int

data KeyInfo a = KeyInfo { kfKey :: a
                         , kfHashfunc :: (a -> Int32)
                         , kfHash :: Int32
                         , kfBitseries :: Int
                         } 

newKeyInfo :: a -> (a -> Int32) -> KeyInfo a
newKeyInfo k hf = KeyInfo { kfKey = k
                                  , kfHashfunc = hf
                                  , kfHash = (hf k)
                                  , kfBitseries = 1
                                  }
 
incBitseries :: KeyInfo a -> KeyInfo a
incBitseries KeyInfo{ kfKey = k
                    , kfHashfunc = hf
                    , kfHash = h
                    , kfBitseries = bs
                    } = KeyInfo {kfKey = k, kfHashfunc = hf, kfHash = h, kfBitseries = succ bs}
                            
                   
