module BoolElimFalse (test) where

import Run (run)
import App
import Test.Hspec
import Lang.Abs  -- for Exp constructors

test :: AppEnv -> [String] -> Spec
test env libs = describe "Bool" $ do
    it "BoolElimFalse" $ do
        program <- readFile "testFiles/Rubric/Bool/elimFalse.afp"
        result <- runAppMStart env (run libs program)
        let expected =
              Right "true : Bool"
        result `shouldBe` expected