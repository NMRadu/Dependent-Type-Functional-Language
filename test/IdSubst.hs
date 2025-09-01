module IdSubst (test) where

import Run (run)
import App
import Test.Hspec
import Lang.Abs  -- for Exp constructors

test :: AppEnv -> [String] -> Spec
test env libs = describe "Id" $ do
    it "IdSubst" $ do
        program <- readFile "testFiles/Rubric/Id/subst.afp"
        result <- runAppMStart env (run libs program)
        let expected =
              Right "zero : Nat"
        result `shouldBe` expected