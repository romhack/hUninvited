module Codec (decode, decodeBinary, buildIndexTable, encode, IndexTable) where

import qualified Data.Bitstream.Lazy      as Bi
import qualified Data.ByteString.Lazy     as Bs
import Data.Bits
import Text.Printf
import Data.Word
import Data.List
import qualified Data.Map                 as M
import Data.Maybe

type IndexTable = [Word8]

charMap :: [String] --table for chars only for print script on screen
charMap = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", 
           "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", " ", "@", 
           "/\n", "\n", "~", "{END}\n", ".", "-", "*", "=", ":", ",", "\"", "!!", 
           "?", ";", "'", "$", "(", ")", "&", "~", "0", "1", "2", "3", "4", "5", 
           "6", "7", "8", "9", "~1", "~2", "~3", "~4", "~5", "%ITEM%\n"]

binaryCharMap :: [Word8] --all tile numbers used in script
binaryCharMap = [0xB1..0xCA]++[0xB0,0x0B,0x0C,0x0D,0x7E,0x0F,0xA1,0xA4,0xA3,0xCB,
                 0xD6,0xD8,0xD0,0x8A,0x8B,0xD7,0xCD,0xCC,0xD9,0xDA,0xA5,0x7E,
                 0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,0x7E,0x7E,
                 0x7E,0x7E,0x4,0x12]

itemByte, eosByte :: Word8
itemByte = 0x12
eosByte = 0x0F

----------------------------------Decode part-----------------------------------

decode :: IndexTable -> [Word8] -> String --decode in binary and then print it as text
decode indexMap input = concat . printScript $ decodeBinary  indexMap input

decodeBinary :: IndexTable -> [Word8] -> [Word8] --decode in binary form
                                              --each bit in tuple has align
                                              --value in stream to the next full byte
decodeBinary indexMap inpt = decodeBinary' $ zip (toBoolStream inpt) (cycle [0,7,6,5,4,3,2,1])
  where
    toBoolStream :: [Word8] -> [Bool] --convert to bytestream and then to bool via bitstream
    toBoolStream wordStream = Bi.unpack (Bi.fromByteString (Bs.pack wordStream) :: Bi.Bitstream Bi.Right)
    decodeBinary' :: [(Bool, Int)] -> [Word8]
    decodeBinary' input  
      --end of message index is 0x1F, stop decode
      | char == eosByte = [char] 
      --item code index in binaryCharMap is 0x3F. 
      | char == itemByte = char : binaryShowItem (byteAlignedStream dropCurrentEntry) ++ decodeBinary' (drop 16 (byteAlignedStream dropCurrentEntry))
      | otherwise    = char : decodeBinary' dropCurrentEntry --3 lo bits + 1 terminator bit for hi bits
        where
          char = binaryCharMap !! fromIntegral  (indexMap !! index)
          index = (hiBits `shiftL` 3) .|. loBits
          hiBits = length $ takeWhile not (map fst input)
          loBits = bitsToByte $ take 3 $ drop (hiBits+1) input
          dropCurrentEntry = drop (hiBits + 4) input
          byteAlignedStream stream = drop (snd (head stream)) stream
          binaryShowItem :: [(Bool, Int)] -> [Word8] -- output 2 whole bytes from stream
          binaryShowItem stream = bitsToByte (take 8 stream) : [bitsToByte (take 8 (drop 8 stream))]

bitsToByte :: (Num a) => [(Bool, Int)] -> a 
bitsToByte = foldl (\by (bi, _) -> by*2 + (if bi then 1 else 0)) 0

printScript :: [Word8] -> [String] --show script on screen
printScript [] = []
printScript (b:bs)
  | b == eosByte = [charMap !! index] --end of message code
  | b == itemByte  = printf "{ITEM 0x%02X 0x%02X}" (head bs) (bs !! 1)  : printScript (drop 2 bs) 
  | otherwise = (charMap !! index) : printScript bs
    where index = fromJust $ elemIndex b binaryCharMap 

-------------------------------------Encode part------------------------------
-- count the number of instances each symbol occurs in a list
-- tuples are swapped, as map had fst as Key, and we should have [(weight, char)] tuples
histogram :: Ord a => [a] -> [(Int,a)]
histogram xs = swap . M.toList $ M.fromListWith (+) [(c, 1) | c <- xs]
  where swap = map (\(a,b)->(b,a))

--histogram should not count item bytes, remove them
removeItemBytes :: (Num a, Eq a) => [a] -> [a]
removeItemBytes [] = []
removeItemBytes (0x12:_:_:bs) = 0x12: removeItemBytes bs
removeItemBytes (b:bs) = b: removeItemBytes bs

buildIndexTable :: [Word8] -> IndexTable --find indexes of sorted charmap in game's charmap and output
buildIndexTable input = map ((fromIntegral . fromJust) . flip elemIndex binaryCharMap) sortedCharMap
  where 
    sortedFrequencies = sort $ histogram (removeItemBytes input)
    sortedCharMap= reverse $ map snd sortedFrequencies--extract only chars from max to min freq

buildCodes :: [Word8] -> [[Bool]] --build a final golomb bitstream codes list out of index value
buildCodes = map (\i -> replicate (fromIntegral (i `shiftR` 3)) False ++ [True] ++ map (testBit (i .&. 7)) [2,1,0])

--encode 1 message with given index table:
--break into item entries, encode plain script, add item bytes and assemble into
--one list
encode :: IndexTable -> [Word8] -> [Word8] 
encode indexTable  = encode' sortedCharMap
  where 
    sortedCharMap =  map ((binaryCharMap !!) . fromIntegral) indexTable --get tile values back out of index table
    encode' sortedCharMap' msg 
      |null rest = toBitStream text --encode plain text without items
      |otherwise  = toBitStream (text ++ [itemByte]) ++ [rest!!1, rest!!2] ++ encode' sortedCharMap' (drop 3 rest)
      where (text, rest) = break (==itemByte) msg --break at item flag byte
            toBitStream input = Bs.unpack $ Bi.toByteString (Bi.pack (concatMap charToCode input) :: Bi.Bitstream Bi.Right)
              where charToCode char = codes !! fromJust (elemIndex char sortedCharMap')
                    codes = buildCodes [0..fromIntegral(length sortedCharMap')]

