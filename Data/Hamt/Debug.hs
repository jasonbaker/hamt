module Data.Hamt.Debug where
import Debug.Trace

debug :: Show a => String -> a -> a
debug name val = trace ("\n" ++ name ++ "=" ++ (show val) ++ "\n") val

