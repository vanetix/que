module QueSpec
  ( main
  , spec
  ) where

import Data.Time.Clock (getCurrentTime)
import Test.Hspec

import qualified Que

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "new" $ do
    it "should create Todo" $ do
      todos <- new
      length todos `shouldBe` 1
  describe "done" $ do
    it "should mark todo complete" $ do
      t <- getCurrentTime
      todos <- new
      let Right (Que.Todo {Que.completed = Just done}:[]) = Que.done t 1 todos
      done `shouldBe` t
  describe "remove" $ do
    it "should remove todo" $ do
      todos <- new
      (length $ Que.remove 1 todos) `shouldBe` 0

new :: IO Que.Todos
new = do
  t <- getCurrentTime
  return $ Que.new t "work" []
