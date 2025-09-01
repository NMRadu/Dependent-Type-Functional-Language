module NatElimZero (test) where

import Run (run)
import App
import Test.Hspec
import Lang.Abs  -- for Exp constructors

test :: AppEnv -> [String] -> Spec
test env libs = describe "Nat" $ do
    it "NatElimZero" $ do
        program <- readFile "testFiles/Rubric/Nat/elimZero.afp"
        result <- runAppMStart env (run libs program)
        let expected =
              Right "true : Bool"
        result `shouldBe` expected
