{-# LANGUAGE DoAndIfThenElse #-}
-- 
module SpellChecker (do_spellcheck) where
-- Importo mis módulos, 
-- se podría decir que este es el 'main'
import Dictionary
import Document
import CommandLine
import Control.Exception
import System.IO

-- La funcion 'do_spellcheck' es la funcion que se encarga de manejar
-- el proceso de chequeo ortografico. Esto incluye, cargar el diccionario,
-- abrir el archivo a procesar, procesar el archivo y luego guardar el
-- diccionario y el archivo de entrada ya procesado.
-- Toma como argumento los argumentos de linea de comando de tipo 'Params'.
do_spellcheck :: Params -> IO () 
do_spellcheck (Params inputPath dictPath) = 
    do
        dict_added_w <- dict_load dictPath
        document <- doc_open inputPath "output.txt"
        dict_to_save <- process_document document dict_added_w dict_new
        dict_save dictPath dict_to_save
        doc_close document
        putStrLn "Procesamiento completo. Ver resultado en:"
        print(dictPath)
        print("output.txt")
        return ()

-- La funcion 'process_document' ejecuta el proceso de chequeo ortografico.
-- Para ello, procesa el archivo palabra por palabra, copiandolas al archivo
-- de salida y consultando al usuario sobre que accion realizar ante una
-- palabra desconocida.
-- Cuando se termina de procesar el archivo, lo cual es señalizado mediante
-- una excepcion por 'doc_get_word', retorna el diccionario (el cual puede
-- haber sido modificado) para guardarlo.
process_document :: Document ->
                    Dictionary ->
                    Dictionary ->
                    IO Dictionary
process_document (Document f_in f_out) dict_added_w dict_ignored_w = 
	do
        catch (
            do
                word_to_process <- doc_get_word (Document f_in f_out)
                let already_in_d = dict_contains word_to_process dict_added_w
                the_last_dictionary <- if already_in_d then
                    do
                        doc_put_word word_to_process (Document f_in f_out)
                        d' <- process_document (Document f_in f_out) dict_added_w dict_ignored_w
                        return (d')
                else 
                    do 
                        (w', d_add', d_ign') <- consult_user word_to_process dict_added_w dict_ignored_w
                        doc_put_word w' (Document f_in f_out)
                        d' <- process_document (Document f_in f_out) d_add' d_ign'
                        return (d')
                return the_last_dictionary) handleException
            where 
                handleException :: SomeException -> IO Dictionary
                handleException _   = do 
                                        return dict_added_w

clearScreen :: IO [()]
clearScreen = sequence (replicate 80 (putChar '\n'))


printMenu :: Word -> IO ()
printMenu w = 
    do
        _ <- clearScreen
        putStr "* Palabra: "
        putStr w
        putStr "\n"
        putStr "*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n"
        putStr "* a: Agregar palabra al diccionario   *\n"
        putStr "* i: Ignorar palabra                  *\n"
        putStr "* r: Reemplazar palabra               *\n"
        putStr "*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n"
        putStr "Ingrese una opcion y despues [Intro]   \n"
        hFlush stdout

-- Verifica si una palabra es conocida, en cuyo caso, continua
-- con el procesamiento del archivo, sin realizar ninguna accion.
-- Si la palabra no es conocida, pregunta al usuario que accion
-- realizar con la misma. Las acciones pueden ser aceptar, ignorar
-- o reemplazar.
consult_user ::  Word -> Dictionary -> Dictionary -> IO (Word, Dictionary, Dictionary)
consult_user w dict_added_w dict_ignored_w = 
    do
        printMenu w
        action <- getLine
        case action of
            "a" -> do
                hFlush stdout
                putStr "Palabra agregada. Presione [Intro] para continuar\n"
                _ <- getLine -- wait for response user! 
                return (w, (dict_add w dict_added_w), dict_ignored_w)
            "i" -> do
                hFlush stdout
                putStr "Palabra agregada. Presione [Intro] para continuar\n"
                _ <- getLine
                return (w, dict_added_w, (dict_add w dict_ignored_w))
            "r" -> do
                hFlush stdout
                putStr "Ingrese el reemplazo de la palabra  \n"
                word_to_replace <- getLine
                putStr "Palabra agregada. Presione [Intro] para continuar\n"
                _ <- getLine
                return (word_to_replace, dict_added_w, dict_ignored_w)
            _ -> do
                putStr "Opción inválida presione [Intro] para volver al menú\n"
                hFlush stdout
                _ <- getLine
                (w', d_add, d_ign) <- consult_user w  dict_added_w dict_ignored_w
                return (w', d_add, d_ign)