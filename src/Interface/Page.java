/**
 * Interface Page
 * Interface qui définit les méthodes communes à toutes les pages du jeu
 */
public interface Page {
    /**
     * Renvoie le panneau (JPanel) de la page
     * @return Le JPanel représentant la page
     */
    javax.swing.JPanel getPanel();

    /**
     * Gère les entrées utilisateur sur la page
     */
    void gererEntree();

    /**
     * Met à jour l'affichage de la page
     */
    void mettreAJour();
}