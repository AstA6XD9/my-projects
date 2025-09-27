package allumettes;

public class StrategieRapide implements Strategie {
    @Override
    public int getPrise(Jeu jeu) {
        int prise = Math.min(Jeu.PRISE_MAX, jeu.getNombreAllumettes());
        if (prise == 0) {
        	prise = 1;
        }
        return prise;
    }
}
