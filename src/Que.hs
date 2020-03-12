{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Que
  ( Todo(..)
  , Todos
  , done
  , load
  , empty
  , new
  , display
  , remove
  , save
  ) where

import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.List (find, transpose)
import Data.Sequence (Seq, (|>), deleteAt, lookup, mapWithIndex, update)
import qualified Data.Sequence as Sequence
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone, utcToLocalTime)
import GHC.Generics (Generic)
import Prelude hiding (lookup)
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import Text.PrettyPrint.Boxes (Box, hsep, left, render, text, vcat)

import Data.Yaml
  ( FromJSON
  , ToJSON
  , decodeFileEither
  , encodeFile
  , prettyPrintParseException
  )

import System.Console.ANSI
  ( Color(..)
  , ColorIntensity(..)
  , ConsoleIntensity(..)
  , ConsoleLayer(..)
  , SGR(..)
  , setSGRCode
  )

type Todos = Seq Todo

data Todo =
  Todo
    { body :: Text
    , created :: UTCTime
    , completed :: Maybe UTCTime
    }
  deriving (Generic, Show)

instance FromJSON Todo

instance ToJSON Todo

configPath :: IO FilePath
configPath = do
  home <- Directory.getHomeDirectory
  pure $ FilePath.joinPath [home, ".todo.yml"]

load :: IO (Either String Todos)
load = do
  decodeResult <- configPath >>= decodeFileEither
  pure $ first (prettyPrintParseException) decodeResult

save :: Todos -> IO (Maybe String)
save todos = do
  path <- configPath
  encodeFile path todos
  pure Nothing

empty :: Todos
empty = Sequence.empty

new :: UTCTime -> String -> Todos -> Todos
new time str todos = todos |> t
  where
    t = Todo {body = pack str, created = time, completed = Nothing}

remove :: Int -> Todos -> Todos
remove i = deleteAt $ unhumanizeId i

done :: UTCTime -> Int -> Todos -> Either String Todos
done time todoId todos =
  case found of
    Just t -> Right $ update id (completeTodo time t) todos
    Nothing -> Left $ "no todo with id exists " ++ (show id)
  where
    id = unhumanizeId todoId
    found = lookup id todos

completeTodo :: UTCTime -> Todo -> Todo
completeTodo time todo = todo {completed = Just time}

displayTime :: TimeZone -> UTCTime -> String
displayTime tz =
  (formatTime defaultTimeLocale "%x %I:%M %p") . (utcToLocalTime tz)

-- Utility function for building console escape codes
color :: Color -> SGR
color c = SetColor Foreground Vivid c

-- Utility function for building console escape codes
bold :: SGR
bold = SetConsoleIntensity BoldIntensity

humanizeId :: Int -> Int
humanizeId = (+) 1

unhumanizeId :: Int -> Int
unhumanizeId i = i - 1

displayTodo :: TimeZone -> Int -> Todo -> [Box]
displayTodo tz idx Todo {body = b, created = c, completed = comp} =
  [ text $ (setSGRCode [color White, bold]) ++ (show . humanizeId $ idx)
  , text $ (setSGRCode [Reset]) ++ (unpack b)
  , text date
  ]
  where
    date =
      case comp of
        Nothing -> (setSGRCode [color Red, bold]) ++ (displayTime tz c)
        Just d -> (setSGRCode [color Green, bold]) ++ (displayTime tz d)

display :: TimeZone -> Todos -> String
display tz todos = render . hsep 4 left $ vcat left <$> columns
  where
    columns = transpose $ toList $ mapWithIndex (displayTodo tz) todos
