package allumettes;

public class StrategieTricheur implements Strategie {
    private boolean aTriche = false;

    @Override
    public int getPrise(Jeu jeu) {
        if (!aTriche) {
            System.out.println("[Je triche...]");
            aTriche = true;
            
            // En mode confiant seulement
            if (!(jeu instanceof Procuration)) {
                try {
                    int aRetirer = jeu.getNombreAllumettes() - 2;
                    jeu.retirer(aRetirer);
                    System.out.println("[Allumettes restantes : 2]");
                } catch (CoupInvalideException e) {
                    // Ne devrait jamais arriver en mode confiant
                }
            }
        }
        return 1; // Annonce toujours qu'il prend 1
    }
}