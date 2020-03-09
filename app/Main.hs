{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either (fromRight)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone)
import Options.Applicative
import qualified Que

data Command
  = New String
  | List
  | Done Int
  | Drop Int

data Options =
  Options Command

main :: IO ()
main =
  run =<<
  execParser
    (info (helper <*> parseOptions) $
     fullDesc <>
     progDesc "A small utility for managing todos" <> header "que - a local task manager")

parseOptions :: Parser Options
parseOptions = Options <$> parseCommand

parseNew :: Parser Command
parseNew = New <$> strArgument (metavar "\"CONTENT\"")

parseList :: Parser Command
parseList = pure List

parseDone :: Parser Command
parseDone = Done <$> argument auto (metavar "ID")

parseDrop :: Parser Command
parseDrop = Drop <$> argument auto (metavar "ID")

parseCommand :: Parser Command
parseCommand =
  subparser $
  command "new"  (info parseNew $ progDesc "create a new item") <>
  command "list" (info parseList $ progDesc "list all items") <>
  command "done" (info parseDone $ progDesc "complete an item") <>
  command "drop" (info parseDrop $ progDesc "drop an item by id")

run :: Options -> IO ()
run (Options cmd) = do
  tz <- getCurrentTimeZone
  loadedTodos <- Que.load
  newTodos <- runCommand cmd $ fromRight [] loadedTodos
  _ <- Que.save newTodos
  putStrLn $ Que.display tz newTodos

runCommand :: Command -> Que.Todos -> IO Que.Todos
runCommand cmd todos = do
  t <- getCurrentTime

  case cmd of
    New content ->
      pure $ Que.new t content todos
    Done i ->
      case Que.done t i todos of
        Left msg -> do
          putStrLn msg
          pure $ todos
        Right newTodos ->
          pure $ newTodos
    Drop i -> do
      pure $ Que.remove i todos
    _ ->
      pure todos
