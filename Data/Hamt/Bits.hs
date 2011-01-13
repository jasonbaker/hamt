module Data.Hamt.Bits where
import Data.Bits
import Data.Int
import Data.Word

popCount :: Int -> Int
popCount x =
    let x1 = x - ((x `shiftR` 1) .&. 0x55555555)
        x2 = ((x1 `shiftR` 2) .&. 0x33333333) + (x1 .&. 0x33333333)
        x3 = ((x2 `shiftR` 4) + x2) .&. 0x0f0f0f0f
        x4 = x3 + (x3 `shiftR` 8)
        x5 = x4 + (x4 `shiftR` 16)
    in x5 .&. 0x0000003f

mask :: Int -> Word 
mask 1 = 0xf8000000
mask 2 = 0x07c00000
mask 3 = 0x003e0000
mask 4 = 0x0001f000
mask 5 = 0x00000f80
mask 6 = 0x0000007c
mask 7 = 0x00000003

getSubkey :: Int32 -> Int -> Int32
getSubkey key bitseries = {-# SCC "GetSubkey" #-} (key `shiftR`  (32 -(bitseries * 5))) .&. 0x01f

setBits :: [Int] -> Int
setBits bitlist = foldl setBit 0 bitlist

