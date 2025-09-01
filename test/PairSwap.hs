module PairSwap (test) where

import Run (run)
import App
import Test.Hspec
import Lang.Abs  -- for Exp constructors

test :: AppEnv -> [String] -> Spec
test env libs = describe "Pair" $ do
    it "PairSwap" $ do
        program <- readFile "testFiles/Rubric/Pair/swap.afp"
        result <- runAppMStart env (run libs program)
        let expected =
              Right "((((pair Bool) Nat) false) zero) : ((Pair Bool) Nat)"
        result `shouldBe` expected
