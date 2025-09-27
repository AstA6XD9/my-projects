package allumettes;

public class Joueur {
    private String nom;
    private Strategie strategie;
    private boolean tricheur;

    public Joueur(String nom, Strategie strategie) {
        this.nom = nom;
        this.strategie = strategie;
        this.tricheur = (strategie instanceof StrategieTricheur);
    }

    public String getNom() {
        return nom;
    }

    public int getPrise(Jeu jeu) throws CoupInvalideException {
        return strategie.getPrise(jeu);
    }

    public boolean estTricheur() {
        return tricheur;
    }
}
