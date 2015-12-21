
{-# LANGUAGE DoAndIfThenElse #-}
-- 

module Document where

import System.IO
import Data.Char

type Word = String
data Document = Document Handle Handle

-- Abre los archivos especificados por los paths
-- pasados como argumentos. El primer path repre-
-- senta el archivo de entrada a procesar, y el
-- segundo path representa el archivo de salida
-- donde se guarda el documento ya procesado.
doc_open :: FilePath -> FilePath -> IO Document
doc_open fp1 fp2 = 
    do 
        file_in <- openFile fp1 ReadMode
        file_out <- openFile fp2 WriteMode
        return (Document file_in file_out)

-- Cierra los archivos especificados
doc_close :: Document -> IO ()
doc_close (Document f1 f2) = 
    do
        hClose f1
        hClose f2
        return ()

-- Obtiene una palabra del documento especificado,
-- copiando todos los caracteres no alfabeticos
-- precedentes al archivo de salida.
-- Cuando alcanza el final del documento, lo señaliza
-- con una excepcion.
doc_get_word :: Document -> IO Word
doc_get_word = get_word doc []

-- Funcion recursiva que analiza caracter por caracter.
-- Si el mismo es alfanumerico, lo agrega a la palabra
-- en construccion.
-- Si el mismo no lo es, dependiendo de si existe o no
-- una palabra formada, lo copia a doc_out , o,
-- finaliza la ejecucion de la funcion situando el stream
-- en la posicion anterioir (del archivo de entrada).
-- hGetChar lanzara una excepcion al encontrarse con el
-- final de archivo. La misma sera manejada desde
-- el modulo Spellchecker.
get_word :: Document -> Word -> IO Word
get_word doc word = do
    char <- hGetChar (fst doc)
    if isAlphaNum(char) then
        get_word doc (word ++ [char])
    else if length word == 0 then do
        hPutChar (snd doc) char
        get_word doc word
    else do
        hSeek (fst doc) RelativeSeek (-1)
        return (word)

-- Escribe una palabra en el documento de salida.
doc_put_word :: Word -> Document -> IO ()
doc_put_word word (Document _ file_out) = 
    do
        hPutStr file_out word
        return()
