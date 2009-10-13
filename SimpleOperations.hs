module SimpleOperations where

import SrtParser


-- Shifting Subtitles
shiftsubtitles :: [Subtitle] -> Double -> [Subtitle]
shiftsubtitles ss sec = [shiftsubtitle s sec | s <- ss]

shiftTimes :: [Double] -> Double -> [Double]
shiftTimes [] _ = []
shiftTimes times shift = map (+ shift) times

shiftsubtitle :: Subtitle -> Double -> Subtitle
shiftsubtitle s sec = do
    let times = subtitleTimes s
    Subtitle (subtitleId s) (shiftedtimes) (subtitleLines s) where
        shiftedtimes = shiftTimes (subtitleTimes s) sec