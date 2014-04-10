{-# LANGUAGE CPP #-}
module Net.Bits (module Net.Bits, module Data.Bits, module Data.Word) where

import Data.Bits
import Data.Word
import qualified Numeric


#if __GLASGOW_HASKELL__ < 708
-- GHC 7.8 introduced a new `finiteBitSize` in Data.Bits, deprecating `bitSize`.
-- We emulate it in older versions.
finiteBitSize :: Bits a => a -> Int
finiteBitSize = bitSize
#endif


-- Manipulating bit arrays
-- Warning: this actually uses lazyness :-)

-- 0: least significant (right most in math notation)
array .!. index         = let x = fromIntegral (array `shiftR` (finiteBitSize x * index))
                          in x

b1 `nextTo` b2          = (fromIntegral b1 `shiftL` finiteBitSize b2) .|. fromIntegral b2



-- catBits xs              = foldr nextTo 0 xs

catBits bs              = cat bs 0
  where cat [] a        = a
        cat (b:bs) a    = cat bs ((a `shiftL` size) .|. fromIntegral b)
        size            = finiteBitSize (head bs)


showHex x               = Numeric.showHex x []
showBin x               = reverse [ if x `testBit` n then '1' else '0' | n <- [0..finiteBitSize x - 1] ]

showHex' n = reverse . take n . (++repeat '0') . reverse . showHex
