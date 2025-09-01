module BoolElimTrue (test) where

import Run (run)
import App
import Test.Hspec
import Lang.Abs  -- for Exp constructors

test :: AppEnv -> [String] -> Spec
test env libs = describe "Bool" $ do
    it "BoolElimTrue" $ do
        program <- readFile "testFiles/Rubric/Bool/elimTrue.afp"
        result <- runAppMStart env (run libs program)
        let expected =
              Right "zero : Nat"
        result `shouldBe` expected