{-|
  Description: Contains code to read and manipulate home-manager’s generations
Contains code to read and manipulate home-manager’s generations
-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.HMGenerations
  ( readGenerations
  , removeGeneration
  , GenerationLine
  , activateGeneration
  )
where

import           Control.Lens          (folded, to, (^.), (^?!))
import           Control.Monad         (void)
import           Data.Bifunctor        (first)
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (unpack)
import           Data.Char             (ord)
import           Data.Monoid           (getFirst)
import           Data.Text             (Text, pack)
import           Data.Time.Clock       (UTCTime)
import           Data.Time.Format      (defaultTimeLocale, parseTimeM)
import           Data.Time.LocalTime   (LocalTime, TimeZone, ZonedTime,
                                        getZonedTime, localTimeToUTC,
                                        zonedTimeToUTC, zonedTimeZone)
import           Data.Validation       (Validation (Failure))
import           Data.Void             (Void)
import           GHC.Generics          (Generic)
import           NixManager.Bash       (Arg (LiteralArg), Expr (Command))
import           NixManager.Process    (noStdin, runProcessToFinish)
import           NixManager.Util       (TextualError, addToError, decodeUtf8,
                                        parseSafe, showText)
import           System.Exit           (ExitCode (ExitFailure))
import           Text.Megaparsec       (Parsec, errorBundlePretty, parse,
                                        sepEndBy, takeWhile1P)
import           Text.Megaparsec.Byte  (char, newline, string)
import           Text.Time.Pretty      (prettyTimeAuto)

-- | One home-manager generation line
data GenerationLine = GenerationLine {
    date       :: UTCTime -- ^ The parsed activation date for the generation
  , datePretty :: Text -- ^ The prettified, human-readable date for the generation
  , genId      :: ByteString -- ^ The generation’s id, here as a text, since I wasn’t sure if it’s always numeric
  , path       :: ByteString -- ^ The generation’s path, which is vital for activating it
  } deriving(Show, Generic)

-- | Parsec type for the parser.
type Parser = Parsec Void ByteString

-- | Roughly parse a word
wordParser :: Parser ByteString
wordParser = takeWhile1P Nothing (/= fromIntegral (ord ' '))

-- | Parse anything that's not EOL
nonEolParser :: Parser ByteString
nonEolParser = takeWhile1P Nothing (/= fromIntegral (ord '\n'))

-- | Parse home-manager’s time format
parseTime :: String -> Maybe LocalTime
parseTime = parseTimeM False defaultTimeLocale "%0Y-%m-%d %H:%M"

-- | Parse home-manager’s time format as UTC
generationTimeParser :: TimeZone -> Parser UTCTime
generationTimeParser tz = do
  yymmdd <- wordParser
  void (char (fromIntegral (ord ' ')))
  hhmm      <- wordParser
  localTime <- maybe (fail "wrong date/time format for generation")
                     pure
                     (parseTime (unpack yymmdd <> " " <> unpack hhmm))
  pure (localTimeToUTC tz localTime)

-- | Parse a whole generation line, given the current date, time and time zone
generationLineParser :: ZonedTime -> Parser GenerationLine
generationLineParser now = do
  time <- generationTimeParser (zonedTimeZone now)
  let timePretty = pack (prettyTimeAuto (zonedTimeToUTC now) time)
  GenerationLine time timePretty
    <$> (string " : id " *> wordParser)
    <*> (string " -> " *> nonEolParser)

-- | Parse @home-manager generations@, given the current date, time and time zone
parseGenerations :: ZonedTime -> ByteString -> TextualError [GenerationLine]
parseGenerations now = parseSafe
  (generationLineParser now `sepEndBy` newline)
  "home-manager generations"


-- | Remove a specific generation
removeGeneration :: GenerationLine -> IO ()
removeGeneration genLine = void $ runProcessToFinish noStdin $ Command
  "home-manager"
  ["remove-generations", LiteralArg (genLine ^. #genId . decodeUtf8)]

-- | Activate a specific generation
activateGeneration :: GenerationLine -> IO ()
activateGeneration genLine = void $ runProcessToFinish noStdin $ Command
  ((genLine ^. #path . decodeUtf8) <> "/activate")
  []

-- | Read all generations
readGenerations :: IO (TextualError [GenerationLine])
readGenerations = do
  nowZoned <- getZonedTime
  po <- runProcessToFinish noStdin (Command "home-manager" ["generations"])
  case po ^?! #result . to getFirst . folded of
    ExitFailure code -> pure
      (Failure
        (  "Error executing generations query for home-manager. Exit code was: "
        <> showText code
        <> ". The stderr output was:\n\n"
        <> (po ^. #stderr . decodeUtf8)
        )
      )
    _ -> pure
      (addToError "Couldn't parse generations output: "
                  (parseGenerations nowZoned (po ^. #stdout))
      )
