package allumettes;

public class Arbitre {
    private final Joueur joueur1;
    private final Joueur joueur2;
    private final boolean confiant;

    public Arbitre(Joueur joueur1, Joueur joueur2, boolean confiant) {
        this.joueur1 = joueur1;
        this.joueur2 = joueur2;
        this.confiant = confiant;
    }

        public void arbitrer(Jeu jeu) {
            Jeu jeuPourJoueur = confiant ? jeu : new Procuration(jeu);
            Joueur joueurCourant = joueur1;
        

            while (jeu.getNombreAllumettes() > 0) {
                System.out.println("Allumettes restantes : " + jeu.getNombreAllumettes());

                try {
                    int prise = joueurCourant.getPrise(jeuPourJoueur);

                    // Validation standard
                    if (prise < 1 || prise > Jeu.PRISE_MAX || prise > jeu.getNombreAllumettes()) {
                        throw new CoupInvalideException(prise,
                            prise < 1 ? "< 1" :
                            prise > Jeu.PRISE_MAX ? "> " + Jeu.PRISE_MAX :
                            "> " + jeu.getNombreAllumettes());
                    }

                    System.out.println(joueurCourant.getNom() + " prend " + prise + 
                                     (prise > 1 ? " allumettes." : " allumette."));
                    jeu.retirer(prise);
                    joueurCourant = (joueurCourant == joueur1) ? joueur2 : joueur1;
                    System.out.println();
                    if (joueur1.estTricheur() || joueur2.estTricheur()) {
                    	System.out.println("Abandon de la partie car " + (joueur1.estTricheur() == true ? joueur1.getNom() : joueur2.getNom() )+ " triche !");
                    			return;
                    }

                } catch (CoupInvalideException e) {
                    System.out.println("Impossible ! " + e.getMessage());
                }
            }

            System.out.println(joueurCourant.getNom() + " perd !");
            System.out.println((joueurCourant == joueur1 ? joueur2 : joueur1).getNom() + " gagne !");
        }
    }