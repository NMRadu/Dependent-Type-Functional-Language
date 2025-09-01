module IR.IrGen where

import IR.SimplePiConvertor (convertSimplePi)
import IR.FunctionDesugar (desugarFunction)
import IR.LambdaDesugar (desugarLambda)
import IR.BruijnConvertor (convertBruijn)
import IR.InductiveDesugar (desugarInductive)
import IR.NatDesugar (desugarNat)
import Logger
import Control.Monad.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import TypeCheck.Expr

import App
import Lang.Abs
import Lang.Par ( myLexer
                , pProgram )

transform::Program -> Either String Program
transform prog0 = do
    prog1 <- desugarNat prog0
    prog2 <- desugarInductive prog1
    prog3 <- convertSimplePi prog2
    prog4 <- desugarFunction prog3
    prog5 <- desugarLambda prog4
    
    convertBruijn prog4
    
parseAndLog :: String -> AppM (Either String [Stmt])
parseAndLog src = do
  logger <- asks debugLogger
  case pProgram (myLexer src) of
    Left err -> return (Left err)
    Right (Program stmts expr) -> do
      liftIO $ logMessage logger ("Library: " ++ prettyPrintExp [] expr ++ " has been loaded")
      return (Right stmts)


generateProgram:: [String] -> String -> AppM (Either String Program)
generateProgram libraries input = do
    libResults <- mapM parseAndLog libraries
    case sequence libResults of
        Left err -> return (Left err)
        Right libSts -> do 
            case pProgram (myLexer input) of
                Left err  -> return (Left err)
                Right (Program userStmts finalExp)  -> do
                    let allStmts = concat libSts ++ userStmts
                    return (transform (Program allStmts finalExp))