import java.io.EOFException;
import java.io.IOException;
import java.util.Scanner;

import word.Word;
import dictionary.*;
import document.Document;

/**
 * La clase Spellchecker.
 * 
 */
public class Spellchecker {

	/** inputUserScanner Para capturar entrada del usuario. */
	private static Scanner inputUserScanner = new Scanner(System.in);

	/**
	 * Metodo principal. Crea los objetos necesarios para procesar el documento de entrada.
	 *
	 * @param args the arguments
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	public static void main(String[] args) throws IOException {
		FileDictionary dictMain;
		MemDictionary dictIgnored;
		String dictPath = null;
		String docInPath;
		String docOutPath = "out.txt";
		if(args.length < 1) {
			System.out.println("Nro. de argumentos erroneos. Deben ser <documento> <diccionario>.");
			System.exit(1);
		} else if(args.length >= 2) {
			dictPath = args[1];
		} else
			dictPath = "dict.txt";
		docInPath = args[0];
		try {
			dictMain = new FileDictionary(dictPath);
			dictIgnored = new MemDictionary();
			proccessDocument(docInPath, docOutPath, dictMain, dictIgnored);
			dictMain.save();
			System.out.println("El domumento " + docInPath + " ha sido procesado.\nResultados en out.txt");
		} catch (IOException e) {
			e.printStackTrace();
			System.out.println("file " + dictPath + " not found");
		}
	}

	/**
	 * Proccess document.
	 *
	 * @param aInput the a input
	 * @param aOutput the a output
	 * @param aDictMain the a dict
	 * @param aDictIgnored the a dict ignored
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	private static void proccessDocument(String aInput, String aOutput, Dictionary aDictMain, Dictionary aDictIgnored) throws IOException {
		Word word = new Word();
		Document document = null;
		try {
			document = new Document(aInput, aOutput);
			while(true) {
				try {
					word = document.getWord();
					if(!aDictMain.contain(word) && !aDictIgnored.contain(word))
						word = consultUser(word, aDictMain, aDictIgnored);	
					document.putWord(word);
				} catch (EOFException e) {
					break;
				}
			}
			document.close();
			inputUserScanner.close();
		} catch (IOException e) {
			e.printStackTrace();
			System.out.println("file " + aInput + " not found");
		}
	}

	/**
	 * Consulta al usuario .
	 *
	 * @param aWord Palabra a buscar
	 * @param aDictMain Diccionario principal donde busca la palabra
	 * @param aDictIgnored Diccionario de palabras ignoradas donde busca la palabra
	 */
	private static Word consultUser(Word aWord, Dictionary aDictMain, Dictionary aDictIgnored) throws IOException {
		String answer;
		String wordBuffer;
		do {
			System.out.println("Palabra no reconocida " + aWord.getWord() + "\n Aceptar (a) - Ignorar (i) - Reemplazar (r): ");
			answer = inputUserScanner.nextLine();
		} while(!answer.equals("a") && !answer.equals("r") && !answer.equals("i"));
		if(answer.equals("a")) {
			aDictMain.add(aWord);
		} else if(answer.equals("i")) {
			aDictIgnored.add(aWord);
		} else {
			System.out.println("Ingrese la palabra que utilizara como reemplazo: ");
			wordBuffer = inputUserScanner.nextLine();
			aWord.setWord(wordBuffer);
		}
		return aWord;		
	}
}