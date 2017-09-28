module Main where
import           Control.Monad
import           Control.Monad.Loops
import           Data.Binary.Bits.Get
import           Data.Binary.Bits.Put
import qualified Data.Binary.Get      as G
import qualified Data.Binary.Put      as P
import           Data.Bits
import qualified Data.ByteString.Lazy as Bs
import           Data.List
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Word
import           RomhackLib           (BinaryBlockVars (..), CharCode,
                                       DecodeTable, EncodeTable,
                                       PtrTableVars (..), TableEntry (..),
                                       addComments, decodeByTable,
                                       dropDuplicateMsgs, encodeByTable,
                                       readBsTail, readDecodeTable,
                                       readEncodeTable, readW16LePtrTable,
                                       readW8Block, splitByEndCode, writeBlock)
import           System.Environment
import           Text.Printf

charMapVars :: BinaryBlockVars
charMapVars = BinaryBlockVars {blockName = "charMap"
                              , blockOffset = 0x1DC47, blockSize = 0x40
                              , fillByte = 0xFF}
indexTblVars :: BinaryBlockVars
indexTblVars = BinaryBlockVars {blockName = "indexTable"
                              , blockOffset = 0x1E43F, blockSize = 0x40
                              , fillByte = 0xFF}

scriptsVars :: [(PtrTableVars, BinaryBlockVars)] --tables and scripts parameters
scriptsVars =
  [  (PtrTableVars {ptrTableOffset = 0x8010, msgCount = 0x100, ptrBase = 0x10}
    ,BinaryBlockVars {blockName = "script 1", blockOffset = 0x8010
                      , blockSize = 0x3425, fillByte = 0xFF})
    ,(PtrTableVars {ptrTableOffset = 0xC010, msgCount = 0x100, ptrBase = 0x4010}
      ,BinaryBlockVars {blockName = "script 2", blockOffset = 0xC010
                        , blockSize = 0x3000, fillByte = 0xFF})
    ,(PtrTableVars {ptrTableOffset = 0x16010, msgCount = 0x100, ptrBase = 0xC010}
      ,BinaryBlockVars {blockName = "script 3", blockOffset = 0x16010
                        , blockSize = 0x2000, fillByte = 0xFF})]

main :: IO() --just pass arguments to compress or decompress
main = getArgs >>= parse
  where
    parse ["-v"] = putStrLn "hUninvitedv1.3\nGolob codes compression tool\
                            \ for 'Uninvited (U) [!]' game."
    parse ["-d", fileName] = decompress fileName
    parse ["-c", fileName] = compress fileName
    parse ["-k", fileName] = checkScript fileName
    parse _ = putStrLn "Usage:\n\
      \  hUninivted -d <file>  Decompress script from given ROM file.\n\
      \  hUninivted -c <file>  Compress script and insert in given ROM file.\n\
      \  hUninivted -k <file>  Check given script file for excess length.\n\
      \Options:\n\
      \  -h     Show this screen.\n\
      \  -v     Show version."

decompress :: String -> IO()
decompress romName = do -- decompress from given ROM
  decodeTblStr <- readFile "decode.tbl"
  rom <- Bs.readFile romName
  let
    decodeTbl = readDecodeTable decodeTblStr
    charMap = readW8Block rom charMapVars
    indexTbl = readW8Block rom indexTblVars
    decompressScript :: (PtrTableVars, BinaryBlockVars) -> IO()
    decompressScript (ptrTblVars, scriptVars) = do
      let --decopress one script text file
        offsets = readW16LePtrTable rom ptrTblVars
        messages = map (readBsTail rom) offsets
        decodedMsgs = map (decompressMsg charMap indexTbl decodeTbl) messages
        msgStrings = map (decodeByTable decodeTbl) decodedMsgs
        scriptStrings = addComments msgStrings
        outName = printf "%s.txt" $ blockName scriptVars
      writeFile outName $ concat scriptStrings
  mapM_ decompressScript scriptsVars

compress :: String -> IO()
compress outName = do --compress and paste to given ROM
  encodeTblStr <- readFile "encode.tbl"
  script1 <- readFile "script 1.txt"
  script2 <- readFile "script 2.txt"
  script3 <- readFile "script 3.txt"
  let
    encodeTbl = readEncodeTable encodeTblStr
    --charMap = readW8Block rom charMapVars
    --indexTbl = readW8Block rom indexTblVars
    indexTbl =  [0..0x3F] --only charmap will be actually sorted
    charMap =  map snd charHistogram
    charHistogram = histogram $ concat encodedScripts
    encodedScripts = map (encodeByTable encodeTbl) [script1, script2, script3]

    compressScript :: ((PtrTableVars, BinaryBlockVars), [CharCode])-> IO()
    compressScript ((ptrTblVars, scriptVars), scriptBin) = do
      let
        msgsBin = splitByEndCode encodeTbl scriptBin
        encodedMsgs = map (compressMsg charMap indexTbl encodeTbl) msgsBin
        -- search for the same strings and discard them in script block
        (uniqueOffsets, uniqueMsgs) = dropDuplicateMsgs encodedMsgs
        fstMsgPtr = blockOffset scriptVars - ptrBase ptrTblVars + msgCount ptrTblVars * 2
        ptrVals = map (fromIntegral . (+ fstMsgPtr)) uniqueOffsets --calc ptrs
        ptrBlock = Bs.concat $ map (P.runPut . P.putWord16le) ptrVals --build block
        outBlock = ptrBlock `Bs.append` Bs.concat uniqueMsgs
      writeBlock scriptVars outName outBlock
  mapM_ compressScript $ zip scriptsVars encodedScripts
  writeBlock indexTblVars outName $ Bs.pack indexTbl
  writeBlock charMapVars outName $ Bs.pack charMap

checkScript :: String -> IO()
checkScript inputName = do --check given script for maximum line length excess
  input <- readFile inputName
  let
    lenThreshold = 27 --maximum line length without warning
    inputLines = lines input --tags are not counted
    --notComment s = not ("//" `isPrefixOf` s)
    textOnlyList = map replaceTags inputLines
    replaceTags :: String -> String
    replaceTags [] = [] --replace [] tags with single character for count
    replaceTags(i:is) = if i == '[' then i : replaceTags (tail (dropWhile (/= ']') is))
                                    else i: replaceTags is
    excessLines = map (+1) $ findIndices isExcess textOnlyList
    isExcess s = length s > lenThreshold && not ("//" `isPrefixOf` s)
  putStrLn $ "Excessive length lines: " ++ show excessLines


histogram :: Ord a => [a] -> [(Int,a)] --sorted counts of each char occurence in list
histogram xs = sortBy (flip compare) countEntries
  where
    countEntries = swap . M.toList $ M.fromListWith (+) [(c, 1) | c <- xs]
    swap = map (\(a,b)->(b,a))

decompressMsg :: [CharCode] -> [Word8] -> DecodeTable -> Bs.ByteString -> [CharCode]
decompressMsg charMap indexTbl decodeTbl = G.runGet getMsg
  where
    getMsg = do
      chnk <- runBitGet bitGetChunk --unpack chunk, ending item or eos
      case findEntry (last chnk) of
        ControlCode _ _ _ [_, _] -> do --control code with 2 parameters found
          par1 <- G.getWord8
          par2 <- G.getWord8
          rest <- getMsg
          return $ chnk ++ [par1, par2] ++ rest
        _ -> return chnk --eos byte found

    bitGetChunk  :: BitGet [CharCode] --chunk of chars, ending item or eol
    bitGetChunk  = do
      hiBoolList <- whileM (not <$> getBool) $ return False --consecutive falses
      lo <- getWord8 3
      let
        hi = length hiBoolList
        index = (hi `shiftL` 3) .|. fromIntegral lo
        char = charMap !! fromIntegral  (indexTbl !! index)
      case findEntry char of
        EndToken {} -> return [char]
        ControlCode _ _ _ [_, _] -> return [char] --disrupt chunk in case of item
        _ -> (char :) <$> bitGetChunk

    findEntry :: CharCode -> TableEntry
    findEntry c = fromMaybe (error (printf "Char with code 0x%02X not \
                                      \found in table" c)) (lookup c decodeTbl)

compressMsg :: [CharCode] -> [Word8] -> EncodeTable -> [CharCode] -> Bs.ByteString
compressMsg charMap indexTbl encodeTbl = P.runPut. runBitPut. putMsg
  where
    putMsg :: [CharCode] -> BitPut ()
    putMsg [] = return () --message ended
    putMsg (c : cs)
      | c `elem` itemCodes = do --item found, dump raw params in out stream
        let (par1:par2:css) = cs
        bitPutChar  c
        joinPut $ P.putWord8 par1
        joinPut $ P.putWord8 par2
        putMsg css
      | otherwise =  bitPutChar c >> putMsg cs --usual char, dump it's binary code

    itemCodes = foldl getItem [] encodeTbl
    getItem acc t = case t of --get all item codes from encode table
      (_, ControlCode c _ _ [_, _]) -> c : acc
      _ -> acc

    bitPutChar :: CharCode -> BitPut ()
    bitPutChar c = do --dump char to binary code stream
      let
        charCode = fromIntegral $ fromMaybe (error "Code not found in charMap")
                                    (elemIndex c charMap)
        index = fromIntegral $ fromMaybe (error "Not found in indexTbl")
                                    (elemIndex charCode indexTbl)
        lo = index .&. 7
        hiVal = fromIntegral $ index `shiftR` 3
      _ <- replicateM hiVal $ putBool False
      putBool True
      putWord8 3 lo
