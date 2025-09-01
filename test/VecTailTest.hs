module VecTailTest (test) where

import Run (run)
import App
import Test.Hspec
import Lang.Abs  -- for Exp constructors

test :: AppEnv -> [String] -> Spec
test env libs = describe "Vec" $ do
    it "VecTailTest  " $ do
        program <- readFile "testFiles/Rubric/Vec/tail.afp"
        result <- runAppMStart env (run libs program)
        let expected =
              Right "(suc zero) : Nat"
        result `shouldBe` expected
