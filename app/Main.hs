module Main where

import Run (run)
import System.Environment (getArgs)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Logger
import App
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (catch, IOException)
import System.Exit (die)

eval :: [String] -> String -> AppM String
eval libraries program = do
    result <- run libraries program
    return $ case result of
        Left err  -> err
        Right val -> show val

loadStaticFiles :: [FilePath] -> IO [String]
loadStaticFiles = mapConcurrently readFile 


main :: IO ()
main = do
    debug_logger <- initLogger True
    let app_env = AppEnv { debugLogger = debug_logger }
    let libraryFiles = ["libraries/Bool.lib", "libraries/Nat.lib", "libraries/Id.lib", "libraries/Vec.lib", "libraries/Either.lib", "libraries/Pair.lib", "libraries/Fin.lib", "libraries/TnB.lib"]
    args <- getArgs
    case args of
        (fileName:_) -> do
            staticContents <- loadStaticFiles libraryFiles
                `catch` \(e :: IOException) ->
                    die $ "Error loading library files: " ++ show e

            program <- readFile fileName
            result <- runAppMStart app_env (run staticContents program)
            case result of
              Left err -> logMessage debug_logger ("The program has encounter an error: " ++ err)
              Right output -> logMessage debug_logger ("\nThe result is: " ++ output)
            logStop debug_logger

