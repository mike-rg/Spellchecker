module Main(main) where

import Control.Monad ( unless )

import System.Console.CmdArgs

import System.IO           ( hPrint, stderr ) 
import System.Exit         ( exitWith, ExitCode(ExitFailure) )

import Prelude hiding ( catch )
import Control.Exception   ( catch, SomeException )

import CommandLine(defaultParams, checkParams, Params)
import SpellChecker

main :: IO ()
main = runCmdLineVersion
        `catch` \(e::SomeException) -> do
                let msg = show e
                unless (msg == "ExitSuccess") $ hPrint stderr msg
                exit r_RUNTIME_ERROR
    where r_RUNTIME_ERROR = 13

exit :: Int -> IO a
exit = exitWith . ExitFailure

runCmdLineVersion :: IO ()
runCmdLineVersion =
 do  clp  <- cmdArgs_ $ defaultParams 
     clpOK <- checkParams clp
     if clpOK
      then do runWithParams clp
      else (putStrLn . show) "Argumentos errones (ejecutar con --help)!!!"

runWithParams:: Params -> IO()
runWithParams p = do_spellcheck p
