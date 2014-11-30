module Codec (decode) where

import qualified Data.Bitstream.Lazy      as Bi
import qualified Data.ByteString.Lazy     as Bs
import Data.Bits
import Text.Printf

endOfMsgCode :: Int
endOfMsgCode = 0x1F
itemCode :: Int
itemCode = 0x3F

charMap :: [String]
charMap = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", 
           " ", "@", "/\n", "\n", "~", "{END}\n", ".", "-", "*", "=", ":", ",", "\"", "!!", "?", ";", "'", "$", "(", ")", "&", "~", 
           "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "~1", "~2", "~3", "~4", "~5", "%ITEM%\n"]
indexMap :: [Int]
indexMap = [0x1A,4,0x13,0xE,0x12,0,8,0xD,0x11,7,0xB,0x14,3,0x1D,0x18,0x20,2,0xC,6,0x1F,5,0xF,0x16,1,0xA,0x2A,0x15,0x25,0x1C,0x26,0x27,0x21,0x17,0x19,0x10,9,0x28,0x3F,0x24,0x2C,0x2D,0x32,0x39,0x29,0x30,0x37,0x38,0x34]

bitsToByte :: [Bool] -> Int
bitsToByte = foldl (\by bi -> by*2 + (if bi then 1 else 0)) 0

showItem :: [Bool] -> String --generate string, showing item + code in plain script
showItem stream = printf "{ITEM 0x%02X 0x%02X}" (bitsToByte (take 8 stream)) (bitsToByte (take 8 (drop 8 stream)))


decode :: Bs.ByteString -> String
decode input = concat $ decode'$ Bi.unpack (Bi.fromByteString input :: Bi.Bitstream Bi.Right)

decode' :: [Bool] -> [String]
decode' input  
  | charIndex == endOfMsgCode = [charMap !! charIndex] --end of message index is 0x1F, stop decode
  | charIndex == itemCode     = showItem (drop skipItemBits input) : decode' (drop (skipItemBits + hiBits + 4 + 16) input)
  | otherwise                 = charMap !! charIndex : decode' (drop (hiBits + 4) input) --3 lo bits + 1 terminator bit for hi bits
    where
      charIndex = indexMap !! index
      index = (hiBits `shiftL` 3) .|. loBits
      hiBits = length $ takeWhile not input 
      loBits = bitsToByte $ take 3 $ drop (hiBits+1) input
      skipItemBits = rem (length input - (hiBits + 4))  8 -- pad leftover of stream to full byte
