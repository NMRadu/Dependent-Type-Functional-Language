module VecHeadTest (test) where

import Run (run)
import App
import Test.Hspec
import Lang.Abs  -- for Exp constructors

test :: AppEnv -> [String] -> Spec
test env libs = describe "Vec" $ do
    it "VecHeadTest" $ do
        program <- readFile "testFiles/Rubric/Vec/head.afp"
        result <- runAppMStart env (run libs program)
        let expected =
              Right "(suc (suc zero)) : Nat"
        result `shouldBe` expected
