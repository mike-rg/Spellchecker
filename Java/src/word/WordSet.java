/*
 * Representa un conjunto de Palabras que va a ser utilizado en un diccionario.
 */
package word;

import java.util.HashSet;
import java.util.Iterator;

/**
 * La Clase WordSet.
 */
public class WordSet{
	
	/** El contenedor de las Palabras. */
	private HashSet<Word> set;

	/**
	 * Instancia un nuevo conjunto de Palabras.
	 */
	public WordSet() {
		set = new HashSet<Word>();
	}

	/**
	 * Agrega una Palabra al conjunto de Palabras.
	 *
	 * @param aWord Palabra que se va a agregar
	 */
	public void add(Word aWord) {
		set.add(aWord);		
	}

	/**
	 * Verifica si una Palabra esta en el conjunto de Palabras.
	 *
	 * @param aWord Palabra a buscar
	 * @return Devuelve verdadero si la palabra esta en el conjunto de Palabras
	 */
	public boolean contains(Word aWord) {
		return set.contains(aWord);
	}

	/**
	 * Limpia todos los elementos<Palabras> del conjunto de Palabras
	 */
	public void clear() {
		set.clear();
	}

	/**
	 * Iterador del conjunto de Palabras.
	 *
	 * @return Iterardor del conjunto de Palabras
	 */
	public Iterator<Word> iterator() {
		return set.iterator();
	}

	/**
	 * Cantidad de Palabras que tiene el conjunto de Palabras.
	 *
	 * @return Entero que indica el tamanio del conjunto de Palabras
	 */
	public int size() {
		return set.size();
	}

	/**
	 * Devuelve el conjunto de Palabras
	 *
	 * @return El conjunto de Palabras
	 */
	public HashSet<Word> getSet() {
		return set;
	}

	/**
	 * Modifica el conjunto de Palabras por otro conjunto de Palabras.
	 *
	 * @param aSet Conjunto de Palabras
	 */
	public void setSet(HashSet<Word> aSet) {
		set = aSet;
	}
}