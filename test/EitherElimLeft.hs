module EitherElimLeft (test) where

import Run (run)
import App
import Test.Hspec
import Lang.Abs  -- for Exp constructors

test :: AppEnv -> [String] -> Spec
test env libs = describe "Either" $ do
    it "EitherElimLeft" $ do
        program <- readFile "testFiles/Rubric/Either/elimLeft.afp"
        result <- runAppMStart env (run libs program)
        let expected =
              Right "true : Bool"
        result `shouldBe` expected