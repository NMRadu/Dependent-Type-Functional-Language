module TnBBot (test) where

import Run (run)
import App
import Test.Hspec
import Lang.Abs  -- for Exp constructors

test :: AppEnv -> [String] -> Spec
test env libs = describe "TnB" $ do
    it "TnBBot" $ do
        program <- readFile "testFiles/Rubric/TnB/bot.afp"
        result <- runAppMStart env (run libs program)
        let expected =
              Right "\\A -> \\fin0 -> ((magic A) match fin0 with\n) : (A : U 0) -> (_ : (Fin zero)) -> A"
        result `shouldBe` expected
