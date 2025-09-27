import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/**
 * Page de démarrage du jeu
 */
public class PageDemarrage implements Page {
    private Jeu jeu;
    private JPanel panel;

    /**
     * Constructeur de la page de démarrage
     * @param jeu Instance du jeu
     */
    public PageDemarrage(Jeu jeu) {
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

        // Panel du logo
        JPanel logoPanel = new JPanel(new BorderLayout());
        logoPanel.setBackground(Jeu.COULEUR_FOND);

        // Ajout du logo ENSEEIHT
        JLabel logoLabel = new JLabel("Karaoké ENSEEIHT", SwingConstants.CENTER);
        logoLabel.setFont(new Font("Arial", Font.BOLD, 40));
        logoLabel.setForeground(Jeu.COULEUR_ACCENT);

        // Panel "N7"
        JPanel n7LogoPanel = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                super.paintComponent(g);
                Graphics2D g2d = (Graphics2D) g.create();
                g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                // Fond du logo
                g2d.setColor(new Color(115, 24, 44)); // Couleur N7 (rouge bordeaux)
                g2d.fillRoundRect(10, 10, getWidth() - 20, getHeight() - 20, 15, 15);

                // Texte "N7"
                g2d.setColor(Color.WHITE);
                g2d.setFont(new Font("Arial", Font.BOLD, 40));
                FontMetrics fm = g2d.getFontMetrics();
                String text = "N7";
                int textWidth = fm.stringWidth(text);
                int textHeight = fm.getHeight();
                g2d.drawString(text, (getWidth() - textWidth) / 2,
                        (getHeight() + textHeight / 2) / 2);

                g2d.dispose();
            }

            @Override
            public Dimension getPreferredSize() {
                return new Dimension(80, 80);
            }
        };
        n7LogoPanel.setBackground(Jeu.COULEUR_FOND);

        logoPanel.add(n7LogoPanel, BorderLayout.NORTH);
        logoPanel.add(logoLabel, BorderLayout.CENTER);

        // Panneau pour les boutons
        JPanel boutonsPanel = new JPanel();
        boutonsPanel.setLayout(new GridLayout(4, 1, 10, 15));
        boutonsPanel.setBackground(Jeu.COULEUR_FOND);
        boutonsPanel.setBorder(new EmptyBorder(20, 100, 20, 100));

        // Création des boutons
        JButton jouerButton = creerBoutonStylise("Jouer au Karaoké");
        JButton choixMusiqueButton = creerBoutonStylise("Choisir une Musique");
        JButton optionsButton = creerBoutonStylise("Options");
        JButton quitterButton = creerBoutonStylise("Quitter");

        boutonsPanel.add(jouerButton);
        boutonsPanel.add(choixMusiqueButton);
        boutonsPanel.add(optionsButton);
        boutonsPanel.add(quitterButton);

        // Ajout des actions aux boutons
        jouerButton.addActionListener(e -> jeu.changerPage(jeu.pageJeuKaraoke));
        choixMusiqueButton.addActionListener(e -> jeu.changerPage(jeu.pageChoixMusique));
        optionsButton.addActionListener(e -> jeu.changerPage(jeu.pageParametres));
        quitterButton.addActionListener(e -> System.exit(0));

        panel.add(logoPanel, BorderLayout.NORTH);
        panel.add(boutonsPanel, BorderLayout.CENTER);
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


}