module EitherElimRight (test) where

import Run (run)
import App
import Test.Hspec
import Lang.Abs  -- for Exp constructors

test :: AppEnv -> [String] -> Spec
test env libs = describe "Either" $ do
    it "EitherElimRight" $ do
        program <- readFile "testFiles/Rubric/Either/elimRight.afp"
        result <- runAppMStart env (run libs program)
        let expected =
              Right "false : Bool"
        result `shouldBe` expected