{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    (
      Todo (..),
      Todos,
      done,
      load,
      new,
      remove,
      save
    ) where

import Data.List (find, filter)
import Data.Bifunctor (first)
import Data.Text (pack, Text)
import Data.Time.Clock (UTCTime)
import Data.Yaml (decodeFileEither, encodeFile, FromJSON, ToJSON, prettyPrintParseException)
import GHC.Generics (Generic)
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath


type Todos = [Todo]

data Todo = Todo {
  tId :: Int
  , body :: Text
  , created :: UTCTime
  , completed :: Maybe UTCTime
} deriving (Generic, Show)

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

new :: UTCTime -> String -> Todos -> Todos
new time str todos = t : todos
  where t = Todo { tId = length todos + 1, body = pack str, created = time, completed = Nothing }

remove :: Int -> Todos -> Todos
remove i = filter ((/= i) . tId)

done :: UTCTime -> Int -> Todos -> Either String Todos
done time todoId todos =
  let found = find (\t -> (tId t) == todoId) todos
  in
    case found of
      Just t ->
        Right $ completeTodo time t <$> todos
      Nothing ->
        Left "no todo with id exists"

completeTodo :: UTCTime -> Todo -> Todo -> Todo
completeTodo time needle todo
  | (tId needle) == (tId todo) = todo { completed = Just time }
  | otherwise = todo
