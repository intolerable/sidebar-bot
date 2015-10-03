module Bot.Utils where

import Data.Text (Text)
import qualified Data.Text as Text

thousandsFormat :: Integer -> Text
thousandsFormat =
  Text.intercalate "," .
  reverse .
  map Text.reverse .
  Text.chunksOf 3 .
  Text.reverse .
  Text.pack .
  show

humanReadableTime :: Integer -> Text
humanReadableTime diff = mconcat $
  if h > 0
    then
      [ tshow h
      , "h "
      , tshow m
      , "m" ]
    else
      [ tshow m
      , "m" ]
  where (h, m) = (diff `div` 60) `divMod` 60

tshow :: Show a => a -> Text
tshow = Text.pack . show
