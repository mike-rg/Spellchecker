/*
 * Representa un diccionario de Palabras.
 */
package dictionary;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import word.*;

/**
 * La Clase Dictionary.
 */
public abstract class Dictionary {
	
	/** Conjuno de Palabras del diccionario. */
	WordSet set;

	/**
	 * Instancia un nuevo diccionario.
	 */
	public Dictionary() {
		set = new WordSet();
	}

	/**
	 * Agrega una palabra al conjunto de Palabras del diccionario.
	 *
	 * @param aWord Palabra a agregar
	 */
	public void add(Word aWord) {
		set.add(aWord);
	}

	/**
	 * Verifica si encuentra una palabra en el conjunto de Palabras
	 * del diccionario.
	 *
	 * @param aWord Palabra a buscar
	 * @return Verdadero si la palabra se encuentra
	 */
	public boolean contain(Word aWord) {
		return set.contains(aWord);
	}

	/**
	 * Vacia el conjunto de Palabras del diccionario.
	 */
	public void clear() {
		set.clear();
	}

	/**
	 * From string list.
	 *
	 * @param list the list
	 */
	public void fromStringList(List<String> list) {
		Iterator<String> iter = list.iterator();

		while (iter.hasNext()) {
			Word word = new Word(iter.next());
			set.add(word);
		}
	}

	/**
	 * To string list.
	 *
	 * @return the list
	 */
	public List<String> toStringList() {
		List<String> list = new ArrayList<String>();
		Iterator<Word> iter = set.iterator();

		while (iter.hasNext()) {
			list.add(iter.next().getWord());
		}
		return list;
	}

	/**
	 * Tamanio del conjunto de Palabras del diccionario.
	 *
	 * @return Entero que indica el tamanio del conjunto de Palabras del diccionario
	 */
	public int size() {
		return set.size();
	}

	/**
	 * Devuelve el conjunto de Palabras del diccionario.
	 *
	 * @return El conjunto de Palabras
	 */
	public WordSet getSet() {
		return set;
	}

	/**
	 * Modifica el conjunto de Palabras del diccionario.
	 *
	 * @param aSet Nuevo conjunto de Palabras
	 */
	public void setSet(WordSet aSet) {
		set = aSet;
	}
}