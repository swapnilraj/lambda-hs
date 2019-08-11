module Main where

import AST
import Parser
import Eval

import Control.Monad.Trans
import System.Console.Haskeline

type Repl a = InputT IO a

process :: String -> IO ()
process line = do
    let e = parseExpr line
    case e of
      Left err -> print err
      Right exp -> print $ runEval exp

repl :: Repl ()
repl = do
  minput <- getInputLine "λ "
  case minput of
    Just input -> (liftIO $ process input) >> repl
    Nothing -> outputStrLn "See ya!"

main :: IO ()
main = runInputT defaultSettings repl
