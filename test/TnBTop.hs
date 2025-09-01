module TnBTop (test) where

import Run (run)
import App
import Test.Hspec
import Lang.Abs  -- for Exp constructors

test :: AppEnv -> [String] -> Spec
test env libs = describe "TnB" $ do
    it "TnBTop" $ do
        program <- readFile "testFiles/Rubric/TnB/top.afp"
        result <- runAppMStart env (run libs program)
        let expected =
              Right "tt : Top"
        result `shouldBe` expected
