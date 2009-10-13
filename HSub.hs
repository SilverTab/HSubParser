{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import System.Console.CmdArgs
import Control.Applicative hiding (empty)
import System.Environment
import SrtParser

main :: IO ()
main = do
    n <- getProgName
    m <- cmdArgs "HSub v1, (C) Jean-Nicolas Jolivet 2009" [shift]
    v <- verbosity
    runapp m v

data AppSettings = AppSettings { appMode :: HSubMode
                               , appVerb :: Verbosity
                                } deriving Show

data Verbosity = Quiet | Normal | Verbose
               deriving (Show, Eq, Ord, Enum)


runapp :: HSubMode -> Verbosity -> IO ()
runapp (Shift sec inf outf) v = do
    print ("Running shift! In:" ++ inf ++ " Out: " ++ outf)

    

verbosity :: IO Verbosity
verbosity = do
    norm <- fromEnum <$> isNormal
    loud <- fromEnum <$> isLoud
    return $ toEnum $ norm + loud

data HSubMode = Shift {seconds :: Double, input :: FilePath, out :: FilePath}
            deriving (Data,Typeable,Show)

shift = mode $ Shift
    {seconds = def &= typ "SECONDS" & text "Seconds to shift by" & argPos 0
    ,input = "input.srt" &= typ "INPUTFILE" & text "Source file" & typFile & argPos 1
    ,out = "out.srt" &= typ "OUTPUTFILE" & text "Output file" & typFile & argPos 2
    } &= prog "shift" & text "usage: HSub shift [FLAGS] seconds inputfile outputfile"

