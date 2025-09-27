import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/**
 * Page des paramètres du jeu
 */
public class PageParametres implements Page {
    private Jeu jeu;
    private JPanel panel;
    private JSlider volumeSlider;
    private JComboBox<String> difficulteComboBox;

    /**
     * Constructeur de la page des paramètres
     * @param jeu Instance du jeu
     */
    public PageParametres(Jeu jeu) {
        this.jeu = jeu;
        panel = new JPanel();
        initialiserPanel();
    }

    /**
     * Initialise le panneau de la page
     */
    private void initialiserPanel() {
        panel.setLayout(new BorderLayout(20, 20));
        panel.setBackground(Jeu.COULEUR_FOND);
        panel.setBorder(new EmptyBorder(30, 30, 30, 30));

        // Titre
        JLabel titre = new JLabel("Paramètres", SwingConstants.CENTER);
        titre.setFont(new Font("Arial", Font.BOLD, 32));
        titre.setForeground(Jeu.COULEUR_ACCENT);
        panel.add(titre, BorderLayout.NORTH);

        // Panneau des options
        JPanel optionsPanel = new JPanel();
        optionsPanel.setLayout(new GridLayout(4, 1, 15, 20));
        optionsPanel.setBackground(Jeu.COULEUR_FOND);
        optionsPanel.setBorder(new EmptyBorder(20, 100, 20, 100));

        // Option de volume
        JPanel volumePanel = new JPanel(new BorderLayout(10, 0));
        volumePanel.setBackground(Jeu.COULEUR_FOND);
        JLabel volumeLabel = new JLabel("Volume");
        volumeLabel.setFont(Jeu.POLICE_TITRE);
        volumeLabel.setForeground(Jeu.COULEUR_TEXTE);

        volumeSlider = new JSlider(0, 100, 50);
        volumeSlider.setBackground(Jeu.COULEUR_FOND);
        volumeSlider.setForeground(Jeu.COULEUR_TEXTE);
        volumeSlider.setPaintTicks(true);
        volumeSlider.setPaintLabels(true);
        volumeSlider.setMajorTickSpacing(25);
        volumeSlider.setMinorTickSpacing(5);

        volumePanel.add(volumeLabel, BorderLayout.NORTH);
        volumePanel.add(volumeSlider, BorderLayout.CENTER);

        // Option de difficulté
        JPanel difficultePanel = new JPanel(new BorderLayout(10, 0));
        difficultePanel.setBackground(Jeu.COULEUR_FOND);
        JLabel difficulteLabel = new JLabel("Difficulté");
        difficulteLabel.setFont(Jeu.POLICE_TITRE);
        difficulteLabel.setForeground(Jeu.COULEUR_TEXTE);

        String[] difficultes = {"Facile", "Moyen", "Difficile"};
        difficulteComboBox = new JComboBox<>(difficultes);
        difficulteComboBox.setFont(Jeu.POLICE_NORMALE);

        difficultePanel.add(difficulteLabel, BorderLayout.NORTH);
        difficultePanel.add(difficulteComboBox, BorderLayout.CENTER);

        // Ajouter les éléments au panel d'options
        optionsPanel.add(volumePanel);
        optionsPanel.add(difficultePanel);

        // Bouton Appliquer
        JButton appliquerButton = creerBoutonStylise("Appliquer");
        appliquerButton.addActionListener(e -> {
            modifierVolume(volumeSlider.getValue() / 100.0);
            reglerDifficulte((String) difficulteComboBox.getSelectedItem());
            JOptionPane.showMessageDialog(panel,
                    "Paramètres appliqués",
                    "Confirmation",
                    JOptionPane.INFORMATION_MESSAGE);
        });

        // Bouton retour
        JButton retourButton = creerBoutonStylise("Retour au menu");
        retourButton.addActionListener(e -> jeu.changerPage(jeu.pageDemarrage));

        optionsPanel.add(appliquerButton);
        optionsPanel.add(retourButton);

        panel.add(optionsPanel, BorderLayout.CENTER);
    }

    /**
     * Crée un bouton stylisé façon Spotify
     * @param texte Texte du bouton
     * @return Bouton stylisé
     */
    private JButton creerBoutonStylise(String texte) {
        JButton bouton = new JButton(texte);
        bouton.setFont(Jeu.POLICE_TITRE);
        bouton.setForeground(Jeu.COULEUR_TEXTE);
        bouton.setBackground(Jeu.COULEUR_SECONDAIRE);
        bouton.setFocusPainted(false);
        bouton.setBorderPainted(false);
        bouton.setCursor(new Cursor(Cursor.HAND_CURSOR));

        // Effet de survol
        bouton.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseEntered(MouseEvent e) {
                bouton.setBackground(Jeu.COULEUR_ACCENT);
            }

            @Override
            public void mouseExited(MouseEvent e) {
                bouton.setBackground(Jeu.COULEUR_SECONDAIRE);
            }
        });

        return bouton;
    }

    @Override
    public JPanel getPanel() {
        return panel;
    }

    @Override
    public void gererEntree() {
        // Implémentation de la méthode
    }

    @Override
    public void mettreAJour() {
        // Implémentation de la méthode
    }

    /**
     * Quitte le jeu
     */
    public void quitterJeu() {
        System.exit(0);
    }

    /**
     * Modifie le volume du jeu
     * @param volume Nouveau volume (0.0 à 1.0)
     */
    public void modifierVolume(double volume) {
        System.out.println("Volume modifié à " + volume);
        // Implémentation du changement de volume
    }

    /**
     * Règle la difficulté du jeu
     * @param difficulte Niveau de difficulté
     */
    public void reglerDifficulte(String difficulte) {
        System.out.println("Difficulté modifiée à " + difficulte);
        // Implémentation du changement de difficulté
    }
}