package allumettes;

public class JeuSimple implements Jeu {
    private int allumettes;

    public JeuSimple(int allumettes) {
        this.allumettes = allumettes;
    }

    public int getNombreAllumettes() {
        return this.allumettes;
    }

    public void retirer(int nbAllumettes) throws CoupInvalideException {
    	if (nbAllumettes < 1) {
    		throw new CoupInvalideException(nbAllumettes, "< 1");
    	}
    	else if (nbAllumettes > Jeu.PRISE_MAX ) {
            throw new CoupInvalideException(nbAllumettes, "> " + Jeu.PRISE_MAX);
        } else if (nbAllumettes > this.allumettes) {
        	throw new CoupInvalideException(nbAllumettes, "> " + this.allumettes);
        }
        
        this.allumettes -= nbAllumettes;
    }
}
