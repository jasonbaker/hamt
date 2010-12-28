module Data.Hamt.List (update) where

update :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
update key value list = update' key value list []

update' :: Eq a => a -> b -> [(a, b)] -> [(a, b)] -> [(a, b)]
update' key value ((k, v):assocs) accum = 
    if key == k then
        accum ++ (key, value):assocs
    else
        update' key value assocs ((k, v):accum)
update' key value [] accum = (key, value):accum

