/*
 * Representa una Palabra y es un wrapper sobre el tipo de datos String.
 */
package word;

/**
 * La Clase Word.
 */
public class Word {
	
	/** palabra, atributo de la clase Word. */
	private String word;

	/**
	 * Instancia una nueva Palabra. Contructor por defecto sin parametros.
	 */
	public Word() {
		word = "";
	}

	/**
	 * Instancia una nueva Palabra.
	 *
	 * @param aWord Palabra que se va a guardar como atributo de instancia
	 */
	public Word(String aWord) {
		word = aWord;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return word.hashCode();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object aObj) {
		if (aObj instanceof Word){
			Word w = (Word)aObj;
			return this.word.equals(w.word);
		}
		return false;
	}

	/**
	 * Devuelve la palabra.
	 *
	 * @return La palabra
	 */
	public String getWord() {
		return word;
	}

	/**
	 * Modifica la palabra.
	 *
	 * @param aWord Nueva palabra
	 */
	public void setWord(String aWord) {
		word = aWord;
	}
}