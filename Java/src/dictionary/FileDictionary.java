/*
 * Carga un diccionario desde un archivo.
 */
package dictionary;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * La Clase FileDictionary hereda de Dictionary.
 */
public class FileDictionary extends Dictionary{
	
	/** Direccion del archivo. */
	private String loadPath;

	/**
	 * Instancia un nuevo FileDictionary con una direccion de un archivo
	 * por defecto.
	 */
	public FileDictionary() {
		loadPath = ".\\src\\dict";
	}

	/**
	 * Instancia un nuevo FileDictionary con la direccion de un archivo especifico.
	 *
	 * @param aLoadPath direccion del archivo
	 */
	public FileDictionary(String aLoadPath) {
		loadPath = aLoadPath;
	}

	/**
	 * Cargan el diccionario desde un archivo.
	 *
	 * @param aLoadPath direccion del archivo
	 * @throws IOException Indica si una exception de entrada/salida a ocurrido al intentar
	 * 					   leer el archivo
	 */
	public void load(String aLoadPath) throws IOException {
		ArrayList<String> list = new ArrayList<String>();
		String bufferLine =null;
		BufferedReader buffer = new BufferedReader (new FileReader (aLoadPath));
		while ((bufferLine = buffer.readLine()) != null)
			list.add(bufferLine);
		buffer.close();
		fromStringList(list);
	}

	/**
	 * Guarda el diccionario a un archivo.
	 *
	 * @throws IOException Indica si una exception de entrada/salida a ocurrido al intentar
	 * 					   escribir el archivo
	 */
	public void save() throws IOException {
		BufferedWriter input = new BufferedWriter(new FileWriter(loadPath));
		List<String> list = toStringList();
		Iterator<String> iter = list.iterator();	

		while (iter.hasNext()) {
			input.write(iter.next());
			input.newLine();
		}
		input.flush();
		input.close();
	}

	/**
	 * Guarda el diccionario a un archivo especifico.
	 *
	 * @param aFile nombre del arhivo
	 * @throws IOException Indica si una exception de entrada/salida a ocurrido al intentar
	 * 					   escribir el archivo
	 */
	public void save(String aFile) throws IOException {
		setLoadPath(aFile);
		save();
	}

	/**
	 * Devuelve la direccion del archivo.
	 *
	 * @return the load path
	 */
	public String getLoadPath() {
		return loadPath;
	}

	/**
	 * Modifica la direccion del archivo.
	 *
	 * @param aLoadPath direccion
	 */
	public void setLoadPath(String aLoadPath) {
		loadPath = aLoadPath;
	}
}