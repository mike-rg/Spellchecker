/*
 * Se utiliza para representar: un documento de entrada que va a ser procesado
 * y un documento de salida resultante de procesar el documento de entrada.
 */
package document;

import word.*;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.EOFException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

/**
 * La Clase Documento.
 */
public class Document {
	
	/** Archivo de entrada. */
	private BufferedReader input;
	
	/** Archivo de salida. */
	private BufferedWriter output;

	/**
	 * Instancia un nuevo documento.
	 *
	 * @param aInput nombre del archivo de entrada
	 * @param aOutput nombre del archivo de salida
	 * @throws IOException Indica si una exception de entrada/salida a ocurrido al intentar
	 * 					   leer los archivos de entrada y salida.
	 */
	public Document(String aInput, String aOutput) throws IOException{
		input = new BufferedReader(new FileReader(aInput));
		output = new BufferedWriter(new FileWriter(aOutput));
	}

	/**
	 * Cierra el archivo de entrada y el archivo de salida.
	 *
	 * @throws IOException Indica si una exception de entrada/salida a ocurrido al
	 * 					   intentar cerrar los archivos de entrada y salida.
	 */
	public void close() throws IOException {
		input.close();
		output.close();
	}

	/**
	 * Devuelve una palabra del archivo de entrada.
	 *
	 * @return la palabra
	 * @throws IOException Indica si una excepcion de entrada/salida a ocurrido al
	 * 					   intentar leer del archivo.
	 * @throws EOFException Indica el fin que se ha llegado al final del archivo.
	 */
	public Word getWord() throws IOException, EOFException {
		Word word = new Word();
		StringBuffer buffer = new StringBuffer();
		int c = 0;
		boolean isGetting = false;
		while((c = input.read()) != -1) {
			if(!Character.isLetterOrDigit(c)) {
				if(!isGetting) {
					output.write(c);
				} else
					break;
			} else {
				buffer.append((char) c);
				isGetting = true;
				input.mark(c);
			}
		}
		if(c == -1)
			throw new EOFException();
		input.reset();		
		word.setWord(buffer.toString());
		return word;
	}

	/**
	 * Agrega una palabra al archivo de salida.
	 *
	 * @param aWord palabra a escribir
	 * @throws IOException Indica una excepcion de entrada/salida a ocurrido al intentar
	 * 					   escribir en el archivo.
	 */
	public void putWord(Word aWord) throws IOException {
		output.write(aWord.getWord());
		output.flush();
	}

	/**
	 * Devuelve el archivo de entrada.
	 *
	 * @return el archivo
	 */
	public BufferedReader getInput() {
		return input;
	}

	/**
	 * Modifica el archivo de entrada.
	 *
	 * @param aInput archivo nuevo
	 */
	public void setInput(BufferedReader aInput) {
		input = aInput;
	}

	/**
	 * Devuelve el archivo de salida.
	 *
	 * @return el archivo
	 */
	public BufferedWriter getOutput() {
		return output;
	}

	/**
	 * Modifica el archivo de salida.
	 *
	 * @param aOutput el archivo
	 */
	public void setOutput(BufferedWriter aOutput) {
		output = aOutput;
	}
}