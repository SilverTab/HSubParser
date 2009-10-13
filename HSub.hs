{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import System.Console.CmdArgs

data HSub = Shift {seconds :: Double, input :: FilePath, out :: FilePath}
            deriving (Data,Typeable,Show)

shift = mode $ Shift
    {seconds = def &= typ "SECONDS" & text "Seconds to shift by" & argPos 0
    ,input = "input.srt" &= typ "INPUTFILE" & text "Source file" & typFile & argPos 1
    ,out = "out.srt" &= typ "OUTPUTFILE" & text "Output file" & typFile & argPos 2
    } &= prog "shift" & text "usage: HSub shift [FLAGS] seconds inputfile outputfile"

main = print =<< cmdArgs "HSub v1, (C) Jean-Nicolas Jolivet 2009" [shift]