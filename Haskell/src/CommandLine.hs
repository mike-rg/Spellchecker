--  
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module CommandLine where

import System.Console.CmdArgs

data Params = Params { filename    :: String,
                       dictionary  :: String}
                         deriving (Show, Data, Typeable)

defaultParams :: Annotate Ann
defaultParams = record Params{} 
                [filename       := "" += name "f" += help "Archivo a procesar",
                 dictionary     := "dict.txt"  += name "d" += help "Diccionario a utilizar (default: dict.txt)"]
                += summary "spellchecker v0.0.0, (C) Paradigmas de la Programacion"
                += program "spellchk"

checkParams :: Params -> IO Bool
checkParams p 
            | null (filename p) = 
                do 
                    putStrLn $ unlines ["ERROR: Debe especificar un archivo a procesar.","Ejecuta con --help para ver las opciones de ejecucion."]
                    return False
            | otherwise = return True
