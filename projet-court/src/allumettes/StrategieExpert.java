package allumettes;

public class StrategieExpert implements Strategie {

    public int getPrise(Jeu jeu) throws CoupInvalideException {
        int nombreAllumettes = jeu.getNombreAllumettes();
        int prise;
        int reste = nombreAllumettes %(Jeu.PRISE_MAX + 1);
        switch (reste) {
            case 1:
                prise = 1;
                break;
            case 2:
                prise = 1;
                break;
            case 3:
                prise = (Jeu.PRISE_MAX - 1);
                break;
            case 0:
                prise = Jeu.PRISE_MAX; 
                break;
            default:
                prise = 1;
                break;
        }

        if (prise < 1) {
    		throw new CoupInvalideException(prise, "< 1");
    	}
    	else if (prise > Jeu.PRISE_MAX ) {
            throw new CoupInvalideException(prise, "> " + Jeu.PRISE_MAX);
        } else if (prise > jeu.getNombreAllumettes()) {
        	throw new CoupInvalideException(prise, "> " + jeu.getNombreAllumettes());
        }

        return prise;
    }
}
  

