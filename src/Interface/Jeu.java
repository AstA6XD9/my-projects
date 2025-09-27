import javax.swing.*;
import java.awt.*;

/**
 * Classe principale du jeu
 */
public class Jeu {
    // Constantes de style 
    public static final Color COULEUR_FOND = new Color(18, 18, 18);
    public static final Color COULEUR_TEXTE = new Color(255, 255, 255);
    public static final Color COULEUR_ACCENT = new Color(30, 215, 96); 
    public static final Color COULEUR_SECONDAIRE = new Color(40, 40, 40);
    public static final Font POLICE_TITRE = new Font("Arial", Font.BOLD, 24);
    public static final Font POLICE_NORMALE = new Font("Arial", Font.PLAIN, 14);

    // Les différentes pages du jeu
    public PageDemarrage pageDemarrage;
    public PageParametres pageParametres;
    public PageJeuKaraoke pageJeuKaraoke;
    public PageChoixMusique pageChoixMusique;

    // Page actuelle affichée
    private Page pageActuelle;
    private JFrame fenetre;

    /**
     * Constructeur du jeu
     */
    public Jeu() {
        // Application du look and feel
        appliquerLookAndFeel();

        // Initialisation des pages
        pageDemarrage = new PageDemarrage(this);
        pageParametres = new PageParametres(this);
        pageJeuKaraoke = new PageJeuKaraoke(this);
        pageChoixMusique = new PageChoixMusique(this);

        // La page de démarrage est la première page affichée
        pageActuelle = pageDemarrage;

        // Initialisation de la fenêtre principale
        initialiserFenetre();
    }

    /**
     * Applique un look and feel moderne
     */
    private void appliquerLookAndFeel() {
        try {
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());

            // Configuration des composants UI
            UIManager.put("Panel.background", COULEUR_FOND);
            UIManager.put("Label.foreground", COULEUR_TEXTE);
            UIManager.put("Button.background", COULEUR_SECONDAIRE);
            UIManager.put("Button.foreground", COULEUR_TEXTE);
            UIManager.put("Button.focus", new Color(0, 0, 0, 0));
            UIManager.put("TextField.background", COULEUR_SECONDAIRE);
            UIManager.put("TextField.foreground", COULEUR_TEXTE);
            UIManager.put("TextArea.background", COULEUR_SECONDAIRE);
            UIManager.put("TextArea.foreground", COULEUR_TEXTE);
            UIManager.put("ScrollPane.background", COULEUR_FOND);
            UIManager.put("List.background", COULEUR_SECONDAIRE);
            UIManager.put("List.foreground", COULEUR_TEXTE);
            UIManager.put("ComboBox.background", COULEUR_SECONDAIRE);
            UIManager.put("ComboBox.foreground", COULEUR_TEXTE);
            UIManager.put("Slider.background", COULEUR_FOND);
        } catch (Exception e) {
            System.err.println("Erreur lors de l'application du look and feel: " + e.getMessage());
        }
    }

    /**
     * Initialise la fenêtre principale du jeu
     */
    private void initialiserFenetre() {
        fenetre = new JFrame("Karaoké ENSEEIHT");
        fenetre.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        fenetre.setSize(900, 600);
        fenetre.setMinimumSize(new Dimension(800, 500));
        fenetre.setContentPane(pageActuelle.getPanel());
        fenetre.setLocationRelativeTo(null); // Centre la fenêtre
        fenetre.setVisible(true);
    }

    /**
     * Change la page actuelle
     * @param page La nouvelle page à afficher
     */
    public void changerPage(Page page) {
        pageActuelle = page;
        fenetre.setContentPane(pageActuelle.getPanel());
        fenetre.revalidate();
        fenetre.repaint();
    }

    /**
     * Point d'entrée de l'application
     */
    public static void main(String[] args) {
        // Création et démarrage du jeu
        SwingUtilities.invokeLater(() -> {
            new Jeu();
        });
    }

}