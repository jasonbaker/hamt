module Data.Hamt.Debug where
import Debug.Trace

debug :: Show a => String -> a -> a
debug name val = trace (name ++ "=" ++ (show val) ++ "\n") val

