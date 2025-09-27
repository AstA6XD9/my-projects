package allumettes;

public class Jouer {
    public static void main(String[] args) {
        if (args.length < 2) {
            System.out.println("Usage : java allumettes.Jouer [-confiant] joueur1@strategie joueur2@strategie");
            System.exit(1);
        }

        boolean confiant = args[0].equals("-confiant");
        int offset = confiant ? 1 : 0;

        Joueur joueur1 = creerJoueur(args[offset]);
        Joueur joueur2 = creerJoueur(args[offset + 1]);

        Jeu jeu = new JeuSimple(13);
        Arbitre arbitre = new Arbitre(joueur1, joueur2, confiant);
        arbitre.arbitrer(jeu);
    }

    private static Joueur creerJoueur(String arg) {
        String[] parts = arg.split("@");
        String nom = parts[0];
        String strategie = parts[1];

        switch (strategie) {
            case "humain":
                return new Joueur(nom, new StrategieHumain(nom));
            case "naif":
                return new Joueur(nom, new StrategieNaif());
            case "rapide":
                return new Joueur(nom, new StrategieRapide());
            case "expert":
                return new Joueur(nom, new StrategieExpert());
            case "tricheur":
                return new Joueur(nom, new StrategieTricheur());
            default:
                throw new IllegalArgumentException("Stratégie inconnue : " + strategie);
        }
    }
}
