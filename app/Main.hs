{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Either (fromRight)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone)
import Data.Version (showVersion)
import Options.Applicative
import Paths_que (version)
import qualified Que

data Command
  = New String
  | List
  | Done Int
  | Drop Int
  | Version

data Options =
  Options Command

main :: IO ()
main =
  run =<<
  execParser
    (info (helper <*> parseOptions) $
     fullDesc <>
     progDesc "A small utility for managing todos" <>
     header "que - a local task manager")

parseOptions :: Parser Options
parseOptions = Options <$> parseCommand

parseVersion :: Parser Command
parseVersion = pure Version

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
  command "new" (info parseNew $ progDesc "create a new item") <>
  command "list" (info parseList $ progDesc "list all items") <>
  command "done" (info parseDone $ progDesc "complete an item") <>
  command "drop" (info parseDrop $ progDesc "drop an item by id") <>
  command "version" (info parseVersion $ progDesc "print version and exit")

run :: Options -> IO ()
run (Options Version) = do
  putStrLn $ showVersion version
run (Options cmd) = do
  tz <- getCurrentTimeZone
  loadedTodos <- Que.load
  newTodos <- runCommand cmd $ fromRight Que.empty loadedTodos
  _ <- Que.save newTodos
  putStrLn $ Que.display tz newTodos

runCommand :: Command -> Que.Todos -> IO Que.Todos
runCommand cmd todos = do
  t <- getCurrentTime
  case cmd of
    New content -> pure $ Que.new t content todos
    Done i ->
      case Que.done t i todos of
        Left msg -> do
          putStrLn msg
          pure $ todos
        Right newTodos -> pure $ newTodos
    Drop i -> do
      pure $ Que.remove i todos
    _ -> pure todos
