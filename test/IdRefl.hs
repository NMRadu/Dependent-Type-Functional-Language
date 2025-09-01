module IdRefl (test) where

import Run (run)
import App
import Test.Hspec
import Lang.Abs  -- for Exp constructors

test :: AppEnv -> [String] -> Spec
test env libs = describe "Id" $ do
    it "IdRefl" $ do
        program <- readFile "testFiles/Rubric/Id/refl.afp"
        result <- runAppMStart env (run libs program)
        let expected =
              Right "((refl Nat) zero) : (((Id Nat) zero) zero)"
        result `shouldBe` expected