module SrtParser where

{-
    A module that parses SubRip subtitle files (.srt) files that looks like this:
    ---------------------------------- SRT SAMPLE --------------------------------
    1
    00:00:02,065 --> 00:00:03,557
    Previous on Boston Legal.

    2
    00:00:03,668 --> 00:00:08,335
    I think you remember Reverend Dumdum.
    The reverend is also a lawyer.
    -------------------------------------------------------------------------------
    
    into an Haskel data type that looks something like "Subtitle Int [Double] String"
    where:
    Int is the subtitle ID
    [Double] are the 2 timestamps converted to seconds
    String is the subtitle text
     
    Since most srt files were created on windows, it also takes into consideration the
    new line problem...
-}

import Text.ParserCombinators.Parsec

data Subtitle = Subtitle Int [Double] String
                deriving (Show)


subtitleID :: Parser Int
subtitleID = do
    subid <- many1 digit    -- The ID
    skipMany1 space         -- Skip any useless spaces
    return (read(subid))

newlinechar :: Parser ()
newlinechar = do
    string "\n" <|> string "\r\n" <|> string "\r" <|> string "\n\r" <?> "New Line Char"
    return ()

twonewlines :: Parser ()
twonewlines = do
    count 2 (newlinechar)
    return ()

ending :: Parser ()
ending = do
    twonewlines <|> eof
    return ()

subtitletext :: Parser String
subtitletext = manyTill anyChar (try(ending))

timestamp :: Parser Double
timestamp = do
    hours           <- count 2 digit; char ':'
    minutes         <- count 2 digit; char ':'
    seconds         <- count 2 digit; char ','
    milliseconds    <- count 3 digit
    --skipMany newlinechar
    return ((read hours * 3600.0) + 
            (read minutes * 60.0) +
            (read seconds * 1.0) +
            (read milliseconds / 1000.0))
        
timestamps :: Parser [Double]
timestamps = do
    time1 <- timestamp              -- First timestamp
    string " --> "                  -- Separator
    time2 <- timestamp              -- Second Timestamp
    skipMany space                  -- Optional (0 or more) spaces
    return (time1 : time2 : [])


subtitle :: Parser Subtitle
subtitle = do
    subID <- subtitleID
    subtitleTimes <- timestamps
    subLines <- subtitletext
    return (Subtitle subID subtitleTimes subLines)


subtitles :: Parser [Subtitle]
subtitles = many1 subtitle

parsesrt :: String -> IO [Subtitle]
parsesrt [] = do
    print "No file names were given dude!"
    return []
    
parsesrt file = do
    result <- parseFromFile subtitles file
    case (result) of
        Left err    -> return []
        Right x     -> return x
    