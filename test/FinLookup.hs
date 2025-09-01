module FinLookup (test) where

import Run (run)
import App
import Test.Hspec
import Lang.Abs  -- for Exp constructors

test :: AppEnv -> [String] -> Spec
test env libs = describe "Fin" $ do
    it "FinLookup" $ do
        program <- readFile "testFiles/Rubric/Fin/lookup.afp"
        result <- runAppMStart env (run libs program)
        let expected =
              Right "(suc (suc zero)) : Nat"
        result `shouldBe` expected