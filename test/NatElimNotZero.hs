module NatElimNotZero (test) where

import Run (run)
import App
import Test.Hspec
import Lang.Abs  -- for Exp constructors

test :: AppEnv -> [String] -> Spec
test env libs = describe "Nat" $ do
    it "NatElimNotZero" $ do
        program <- readFile "testFiles/Rubric/Nat/elimNotZero.afp"
        result <- runAppMStart env (run libs program)
        let expected =
              Right "false : Bool"
        result `shouldBe` expected
