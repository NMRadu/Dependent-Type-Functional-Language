module IdTrans (test) where

import Run (run)
import App
import Test.Hspec
import Lang.Abs  -- for Exp constructors

test :: AppEnv -> [String] -> Spec
test env libs = describe "Id" $ do
    it "IdTrans" $ do
        program <- readFile "testFiles/Rubric/Id/trans.afp"
        result <- runAppMStart env (run libs program)
        let expected =
              Right "((refl Nat) zero) : (((Id Nat) zero) zero)"
        result `shouldBe` expected