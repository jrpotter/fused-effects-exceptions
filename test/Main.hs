{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Prelude hiding (ioError)

import           Control.Carrier.Lift (runM)
import qualified Control.Carrier.State.Strict as State
import           Control.Effect.Exception
import           Control.Effect.State
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

problematic :: (Has (Lift IO) sig m, Has (State Char) sig m) => m ()
problematic =
  let throws = modify @Char succ *> throwIO (userError "should explode") `finally` put @Char 'x'
  in throws `catch` (\(_ :: IOException) -> pure ())

testStateDropsWrites :: Tasty.TestTree
testStateDropsWrites = HUnit.testCase "State.Strict drops writes" $ do
  result <- State.execState 'a' problematic
  result HUnit.@?= 'a' -- writes are lost

tests :: Tasty.TestTree
tests = Tasty.testGroup "Control.Carrier.Exception"
  [ Tasty.testGroup "finally"
    [ testStateDropsWrites
    ]
  ]

main :: IO ()
main = Tasty.defaultMain tests
