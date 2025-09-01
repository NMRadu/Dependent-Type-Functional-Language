module VecElimTest (test) where

import Run (run)
import App
import Test.Hspec
import Lang.Abs  -- for Exp constructors

test :: AppEnv -> [String] -> Spec
test env libs = describe "Vec" $ do
    it "VecElimTest" $ do
        program <- readFile "testFiles/Rubric/Vec/elimTest.afp"
        result <- runAppMStart env (run libs program)
        let expected =
              Right "((((cons Nat) (suc (suc zero))) (suc zero)) ((((cons Nat) (suc zero)) (suc (suc zero))) ((((cons Nat) zero) (suc (suc (suc zero)))) (nil Nat)))) : ((Vec Nat) (suc (suc (suc zero))))"
        result `shouldBe` expected
