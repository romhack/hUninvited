module Main where

import Codec
import System.Exit
import System.Environment
import System.Console.GetOpt
import Control.Monad.Error
import qualified Data.ByteString.Lazy as Bs
import Data.Binary.Get
import Data.Int

----------------------------------------Command line parse part----------------------------------

data Action = Decode | Batch | NoAction deriving (Show, Eq)
data Options = Options
              {optHelp :: Bool
              ,optVersion :: Bool
              ,optAction :: Action
              }
              deriving (Show)
defaultOptions :: Options
defaultOptions = Options
                  {optHelp = False
                  ,optVersion = False
                  ,optAction = NoAction
                  }

usage :: String
usage = usageInfo "Usage: hUninvited [-d | -b] file_name pointer_offset pointer_count base_offset" options

options :: [OptDescr (Options -> Options)]
options =
	[ Option "d"  ["decode"]  (NoArg (\opts -> opts {optAction = Decode}))  "decode from ROM. -d <file_name offset>"
        , Option "b"  ["batch"]   (NoArg (\opts -> opts {optAction = Batch}))   "batch decode from ROM. -b <file_name ptr_offset ptr_count base_offset>"
	--,Option "e"  ["encode"]  (NoArg (\opts -> opts {optAction = Encode}))  "encode from raw binary. -e <file_name>"
        , Option "h?" ["help"]    (NoArg (\ opts -> opts { optHelp = True }))   "show help."
        , Option "v"  ["version"] (NoArg (\ opts -> opts { optVersion = True })) "show version number."
	]


deforOpts :: [String] -> IO (Options, [String])
deforOpts argv = 
  case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> ioError (userError (concat errs ++ usage))

----------------------------------------------Main------------------------------------------------------

main :: IO()
main = do
  argv <- getArgs
  (opts, nonOpts) <- deforOpts argv
  when (optVersion opts) $ do
    putStrLn "hUninvited. NES Uninvited script decompressor. Version 0.1"
    exitSuccess
  when (optHelp opts) $ do
    putStrLn usage
    exitSuccess
  let action = optAction opts
  case action of

    Decode -> do --decode one message
      when (length nonOpts /= 2) $ do
        putStrLn "Supply exactly one file name, and one text offset"
        putStrLn usage
        exitFailure
      let [fileName, sOffset] = nonOpts
      input <-  Bs.readFile fileName
      putStrLn $ decode $ Bs.drop (read sOffset :: Int64) input
     
    Batch -> do --decoding full script
      when (length nonOpts /= 4) $ do
        putStrLn "Supply exactly one file name, one pointer table offset, one pointer count and one text base offset"
        putStrLn usage
        exitFailure
      let [fileName, sPtrOffset, sPtrCount, sBaseOffset] = nonOpts
      input <-  Bs.readFile fileName
      let 
        ptrOffset = read sPtrOffset :: Int64
        ptrCount = read sPtrCount :: Int
        baseOffset = read sBaseOffset :: Int
        pointers = map (+ baseOffset) $ map fromIntegral (runGet (replicateM ptrCount getWord16le) (Bs.drop ptrOffset input))
        decodeMsg offset = decode $ Bs.drop (fromIntegral offset) input
      putStrLn $ concatMap decodeMsg pointers
   
    NoAction -> do
      putStrLn "Supply action flag"
      putStrLn usage
      exitFailure 
