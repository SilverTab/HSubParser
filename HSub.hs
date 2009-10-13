{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import System.Console.CmdArgs
import Control.Applicative hiding (empty)
import System.Environment
import SrtParser
import qualified SimpleOperations

main :: IO ()
main = do
    n <- getProgName
    m <- cmdArgs "HSub v1, (C) Jean-Nicolas Jolivet 2009" [shift]
    v <- verbosity
    runapp m v


data Verbosity = Quiet | Normal | Verbose
               deriving (Show, Eq, Ord, Enum)


runapp :: HSubMode -> Verbosity -> IO ()
runapp (Shift sec inf outf) v = do
    pr <- parsesrt inf
    print ("Shifting sub by: " ++ (show sec))
    print (SimpleOperations.shiftsubtitles pr sec)



verbosity :: IO Verbosity
verbosity = do
    norm <- fromEnum <$> isNormal
    loud <- fromEnum <$> isLoud
    return $ toEnum $ norm + loud




-- Command line modes
data HSubMode = Shift {seconds :: Double, input :: FilePath, out :: FilePath}
            deriving (Data,Typeable,Show)

shift = mode $ Shift
    {seconds = 0.0 &= typ "SECONDS" & text "Seconds to shift by"
    ,input = "input.srt" &= typ "INPUTFILE" & text "Source file" & typFile & argPos 0
    ,out = "out.srt" &= typ "OUTPUTFILE" & text "Output file" & typFile & argPos 1
    } &= prog "shift" & text "usage: HSub [FLAGS] -s=SECONDS inputfile outputfile\n  ex: HSub -s=2.5 in.srt out.srt"

