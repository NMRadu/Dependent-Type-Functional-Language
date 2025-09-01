module FinElimTest (test) where

import Run (run)
import App
import Test.Hspec
import Lang.Abs  -- for Exp constructors

test :: AppEnv -> [String] -> Spec
test env libs = describe "Fin" $ do
    it "FinElimTest" $ do
        program <- readFile "testFiles/Rubric/Fin/elimTest.afp"
        result <- runAppMStart env (run libs program)
        let expected =
              Right "true : Bool"
        result `shouldBe` expected