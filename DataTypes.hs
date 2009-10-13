module DataTypes where
-- This module just defines the required datatypes needed...


data Subtitle = Subtitle {subtitleId :: Int, subtitleTimes :: [Double], subtitleLines :: String}
                deriving (Show)