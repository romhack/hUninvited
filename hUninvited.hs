module Main 
where
import Codec
import System.Exit
import System.Environment
import System.Console.GetOpt
import Control.Monad.Error
import qualified Data.ByteString.Lazy as Bs
import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import Data.List.Split
import Data.Word
import Data.Maybe
----------------------------------------Command line parse part----------------------------------

data Action = Decode | Script | Index | Encode | NoAction deriving (Show, Eq)
data Options = Options
              {optHelp :: Bool
              ,optVersion :: Bool
              ,optAction :: Action
              ,optOutput :: Maybe FilePath
              }
              deriving (Show)

defaultOptions :: Options
defaultOptions = Options
                  {optHelp = False
                  ,optVersion = False
                  ,optAction = NoAction
                  ,optOutput = Nothing
                  }

usage :: String
usage = usageInfo "Usage: hUninvited [-d | -s | -i | -e] arguments" options

options :: [OptDescr (Options -> Options)]
options =
	[ Option "d"  ["decode"]  (NoArg (\opts -> opts {optAction = Decode}))   "decode one message from ROM at given offset"
        , Option "s"  ["script"]  (NoArg (\opts -> opts {optAction = Script}))   "batch decode script from ROM by pointer table"
        , Option "i"  ["index"]   (NoArg (\opts -> opts {optAction = Index}))    "build binary index table from given decoded binary files"
        , Option "e"  ["encode"]  (NoArg (\opts -> opts {optAction = Encode}))   "encode decoded binary file with given binary index table file"
	, Option "o"  ["output"]  (OptArg ((\f opts -> opts {optOutput = Just f}) . fromMaybe "output.bin") "FILE") 
          "output to binary FILE" 
        , Option "v"  ["version"] (NoArg (\opts -> opts { optVersion = True })) "output version information"
        , Option "h?" ["help"]    (NoArg (\opts -> opts { optHelp = True }))    "display help"
	]


deforOpts :: [String] -> IO (Options, [String])
deforOpts argv = 
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usage))



----------------------------------------------Main------------------------------------------------------
getIndexMap :: [Word8] -> Int -> IndexTable 
getIndexMap input offset = take 0x30 $ drop offset input--size of index table in ROM: 9bits for encoded 0x2F offset at max.


main :: IO()
main = do
  argv <- getArgs
  (opts, nonOpts) <- deforOpts argv
  when (optVersion opts) $ do
    putStrLn "hUninvited. NES Uninvited text tool. Version 1.0"
    exitSuccess
  when (optHelp opts) $ do
    putStrLn usage
    exitSuccess
  let action = optAction opts
  case action of

    Decode -> do --decode one message
      when (length nonOpts /= 3) $ do
        putStrLn "Supply exactly one file name, index map offset and text offset"
        putStrLn usage
        exitFailure
      let [fileName, sIndexMapOffset, sOffset] = nonOpts
      input <-  Bs.readFile fileName
      let
        inputU8 = Bs.unpack input
        indexMap = getIndexMap inputU8 $ read sIndexMapOffset 
      case optOutput opts of
        Just name -> Bs.writeFile name $ Bs.pack $ decodeBinary indexMap $ drop (read sOffset) inputU8
        Nothing -> putStrLn $ decode indexMap $ drop (read sOffset) inputU8 
     
    Script -> do --decoding full script
      when (length nonOpts /= 5) $ do
        putStrLn "Supply exactly one file name, one index map offset, one pointer table offset, one pointer count and one text base offset"
        putStrLn usage
        exitFailure
      let [fileName, sIndexMapOffset, sPtrOffset, sPtrCount, sBaseOffset] = nonOpts
      input <-  Bs.readFile fileName
      let
        inputU8 = Bs.unpack input
        indexMap = getIndexMap inputU8 $ read sIndexMapOffset
        ptrOffset = read sPtrOffset :: Int64
        ptrCount = read sPtrCount :: Int
        baseOffset = read sBaseOffset :: Int
        pointers = map ((+ baseOffset) . fromIntegral) (runGet (replicateM ptrCount getWord16le) (Bs.drop ptrOffset input))
        decodeMsg offset = decode indexMap $ drop offset inputU8
        decodeMsgBinary offset = decodeBinary indexMap $ drop offset inputU8
      case optOutput opts of
        Just name -> Bs.writeFile name $ Bs.pack $ concatMap decodeMsgBinary pointers
        Nothing -> putStrLn $ concatMap decodeMsg pointers

    Index -> do --create index file from given files of binary plain scripts
      bsList <- mapM Bs.readFile nonOpts
      let input = Bs.concat bsList
          indexTable = buildIndexTable $ Bs.unpack input
          name = fromMaybe "indexTable.bin" $ optOutput opts
      Bs.writeFile name $ Bs.pack indexTable

    Encode -> do --encode file
      when (length nonOpts /= 3) $ do
        putStrLn "Supply decoded binary file name, index table file name and ptr table start address in RAM"
        putStrLn usage
        exitFailure
      let [plainFileName, indexFileName, sBaseAddress] = nonOpts
      input <-  Bs.readFile plainFileName
      indexTable <- Bs.readFile indexFileName
      let
        name = fromMaybe "encoded.bin" $ optOutput opts
        baseAddress = read sBaseAddress
        messages = split (keepDelimsR . dropBlanks $ oneOf [0xF]) $ Bs.unpack input 
        encodedMsgs = map (encode (Bs.unpack indexTable)) messages
        --add text bank address in RAM to pointer table size and appropriate
        --encoded message length for final pointer value
        ptrVals = scanl (+) (baseAddress + length encodedMsgs*2) $ map length $ init encodedMsgs
        ptrTable = map (runPut . putWord16le.fromIntegral) ptrVals
      --ptr and then encoded script go in one file
      Bs.writeFile name $ Bs.concat ptrTable
      Bs.appendFile name $ Bs.pack (concat encodedMsgs)
   
    NoAction -> do
      putStrLn "Supply action flag"
      putStrLn usage
      exitFailure 
