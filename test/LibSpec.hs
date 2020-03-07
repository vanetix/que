module LibSpec (main, spec) where

import Test.Hspec
import Data.Time.Clock (getCurrentTime)

import qualified Lib

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
      let Right (Lib.Todo { Lib.completed = Just done } : []) = Lib.done t 1 todos
      done `shouldBe` t

  describe "remove" $ do
    it "should remove todo" $ do
      todos <- new
      (length $ Lib.remove 1 todos) `shouldBe` 0

new :: IO Lib.Todos
new = do
  t <- getCurrentTime
  return $ Lib.new t "work" []
