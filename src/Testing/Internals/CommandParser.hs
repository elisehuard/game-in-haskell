{-# LANGUAGE OverloadedStrings #-}
module Testing.Internals.CommandParser where

import Testing.GameTypes
import Data.Attoparsec.Text
import Data.Text
import Control.Applicative ((<$>),(<*>), (*>), (<*), (<|>))

commandParser :: Parser Command
commandParser = livesCommandParser <|> playerPosCommandParser

livesCommandParser :: Parser Command
livesCommandParser = LivesCommand <$> ("lives" *> skipSpace *> "=" *> skipSpace *> decimal)

playerPosCommandParser :: Parser Command
playerPosCommandParser = PlayerPosCommand  <$>((,)
                                                <$> ("playerPos" *> skipSpace *> "=" *> skipSpace *> "(" *> signed decimal <* skipSpace <* ",")
                                                <*> (skipSpace *> signed decimal <* skipSpace <* ")"))

parseCommand :: Text -> Either String Command
parseCommand = parseOnly commandParser
