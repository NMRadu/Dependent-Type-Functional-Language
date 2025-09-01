-- Main.hs
module Main where

import Logger
import App
import qualified Test.Hspec as H
import qualified BoolElimFalse
import qualified BoolElimTrue
import qualified EitherElimLeft
import qualified EitherElimRight
import qualified FinLookup
import qualified FinElimTest
import qualified IdRefl
import qualified IdSubst
import qualified IdSym
import qualified IdTrans
import qualified NatElimNotZero
import qualified NatElimZero
import qualified PairSwap
import qualified TnBBot
import qualified TnBTop
import qualified VecHeadTest
import qualified VecTailTest
import qualified VecAppend
import qualified VecElimTest

main :: IO ()
main = do
    logger <- initLogger False
    let libPaths =
          [ "libraries/Bool.lib"
          , "libraries/Nat.lib"
          , "libraries/Id.lib"
          , "libraries/Vec.lib"
          , "libraries/Either.lib"
          , "libraries/Pair.lib"
          , "libraries/Fin.lib"
          , "libraries/TnB.lib"
          ]

    libs <- mapM readFile libPaths
    let env = AppEnv { debugLogger = logger }

    -- Use shared environment for all tests
    H.hspec $ do
        BoolElimFalse.test env libs
        BoolElimTrue.test env libs
        EitherElimLeft.test env libs
        EitherElimRight.test env libs
        FinLookup.test env libs
        FinElimTest.test env libs
        IdRefl.test env libs
        IdSubst.test env libs
        IdSym.test env libs
        IdTrans.test env libs
        NatElimNotZero.test env libs
        NatElimZero.test env libs
        PairSwap.test env libs
        TnBBot.test env libs
        TnBTop.test env libs
        VecHeadTest.test env libs
        VecTailTest.test env libs
        VecAppend.test env libs
        VecElimTest.test env libs

    logStop logger
