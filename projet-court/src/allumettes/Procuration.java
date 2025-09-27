package allumettes;

public class Procuration implements Jeu {
    private final Jeu jeuReel;

    public Procuration(Jeu jeuReel) {
        this.jeuReel = jeuReel;
    }

    @Override
    public int getNombreAllumettes() {
        return jeuReel.getNombreAllumettes();
    }

    @Override
    public void retirer(int nbPrises) throws CoupInvalideException {
        // Convertit OperationInterditeException en CoupInvalideException
        try {
            throw new OperationInterditeException("");
        } catch (OperationInterditeException e) {
            throw new CoupInvalideException(nbPrises, "triche détectée");
        }
    }
}