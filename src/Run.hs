module Run where

import Lang.Abs
-- import TypeCheck.Prog ( infer )

-- import Value ( Value )
-- import Interp.Prog ( interp )
import App

import IR.IrGen (generateProgram)
import TypeCheck.Program(evaluateProgram)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)

import Logger
import Text.Printf (PrintfArg(parseFormat))

run :: [String] -> String -> AppM (Either String String)
run libraries input = do
    transformOutput <- generateProgram libraries input
    case transformOutput of
        Left err -> return $ Left err
        Right prog -> do
            logger <- asks debugLogger
            case evaluateProgram prog of 
                Left err -> return $ Left err
                Right (context, result) -> do
                    liftIO $ logMessage logger ("Context:\n" ++ context)
                    return $ Right result
