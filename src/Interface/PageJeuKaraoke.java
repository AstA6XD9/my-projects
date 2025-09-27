import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Timer;
import java.util.TimerTask;

/**
 * Page du jeu de karaoké
 */
public class PageJeuKaraoke implements Page {
    private Jeu jeu;
    private JPanel panel;
    private int score;
    private boolean enCours;
    private JTextArea parolesArea;
    private JLabel scoreLabel;
    private String chansonActuelle = "Nom de la chanson";
    private JPanel visualiseurPanel;
    private Timer timer;

    /**
     * Constructeur de la page du jeu de karaoké
     * @param jeu Instance du jeu
     */
    public PageJeuKaraoke(Jeu jeu) {
        this.jeu = jeu;
        panel = new JPanel();
        score = 0;
        enCours = false;
        initialiserPanel();
    }

    /**
     * Initialise le panneau de la page
     */
    private void initialiserPanel() {
        panel.setLayout(new BorderLayout(0, 0));
        panel.setBackground(Jeu.COULEUR_FOND);

        // Barre supérieure avec titre et contrôles
        JPanel topBar = new JPanel(new BorderLayout(10, 0));
        topBar.setBackground(new Color(24, 24, 24));
        topBar.setBorder(new EmptyBorder(15, 20, 15, 20));

        JLabel titreLabel = new JLabel(chansonActuelle);
        titreLabel.setFont(new Font("Arial", Font.BOLD, 24));
        titreLabel.setForeground(Jeu.COULEUR_TEXTE);

        JPanel controlsPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
        controlsPanel.setOpaque(false);

        JButton retourButton = new JButton("Retour");
        retourButton.setFont(Jeu.POLICE_NORMALE);
        retourButton.setForeground(Jeu.COULEUR_TEXTE);
        retourButton.setBackground(Jeu.COULEUR_SECONDAIRE);
        retourButton.setBorderPainted(false);
        retourButton.setFocusPainted(false);
        retourButton.addActionListener(e -> {
            arreterKaraoke();
            jeu.changerPage(jeu.pageDemarrage);
        });

        controlsPanel.add(retourButton);
        topBar.add(titreLabel, BorderLayout.WEST);
        topBar.add(controlsPanel, BorderLayout.EAST);

        // Panneau principal
        JPanel mainPanel = new JPanel(new BorderLayout(20, 20));
        mainPanel.setBackground(Jeu.COULEUR_FOND);
        mainPanel.setBorder(new EmptyBorder(20, 20, 20, 20));

        // Zone de visualisation du son
        visualiseurPanel = new JPanel() {
            @Override
            protected void paintComponent(Graphics g) {
                super.paintComponent(g);
                Graphics2D g2d = (Graphics2D) g.create();
                g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

                int width = getWidth();
                int height = getHeight();
                g2d.setColor(Jeu.COULEUR_SECONDAIRE);
                g2d.fillRect(0, 0, width, height);

                // Dessiner des barres de visualisation si le karaoké est en cours
                if (enCours) {
                    int barCount = 50;
                    int barWidth = width / barCount;

                    for (int i = 0; i < barCount; i++) {
                        int barHeight = enCours ? (int) (Math.random() * height / 2) : 10;
                        g2d.setColor(new Color(
                                Math.min(255, (int) (30 + barHeight * 0.8)),
                                Math.min(255, (int) (215 + barHeight * 0.3)),
                                Math.min(255, (int) (96 + barHeight * 0.5))
                        ));

                        int x = i * barWidth;
                        int y = (height - barHeight) / 2;
                        g2d.fillRect(x + 2, y, barWidth - 4, barHeight);
                    }
                }

                g2d.dispose();
            }
        };
        visualiseurPanel.setPreferredSize(new Dimension(0, 150));

        // Zone d'affichage des paroles
        parolesArea = new JTextArea();
        parolesArea.setEditable(false);
        parolesArea.setFont(new Font("Arial", Font.BOLD, 22));
        parolesArea.setForeground(Jeu.COULEUR_TEXTE);
        parolesArea.setBackground(Jeu.COULEUR_SECONDAIRE);
        parolesArea.setLineWrap(true);
        parolesArea.setWrapStyleWord(true);
        parolesArea.setText("Appuyez sur 'Lancer' pour commencer à chanter...");
        parolesArea.setBorder(new EmptyBorder(15, 15, 15, 15));

        JScrollPane scrollPane = new JScrollPane(parolesArea);
        scrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED);
        scrollPane.setBorder(BorderFactory.createEmptyBorder());

        // Zone d'affichage du score
        scoreLabel = new JLabel("Score: 0", SwingConstants.CENTER);
        scoreLabel.setFont(new Font("Arial", Font.BOLD, 28));
        scoreLabel.setForeground(Jeu.COULEUR_ACCENT);
        scoreLabel.setOpaque(true);
        scoreLabel.setBackground(Jeu.COULEUR_SECONDAIRE);
        scoreLabel.setBorder(new EmptyBorder(10, 0, 10, 0));

        // Panneau inférieur avec boutons de contrôle
        JPanel bottomPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 20, 10));
        bottomPanel.setBackground(new Color(24, 24, 24));
        bottomPanel.setBorder(new EmptyBorder(15, 0, 15, 0));

        JButton lancerButton = creerBoutonRond("Lancer");
        JButton arreterButton = creerBoutonRond("Arrêter");

        lancerButton.addActionListener(e -> lancerKaraoke());
        arreterButton.addActionListener(e -> arreterKaraoke());

        bottomPanel.add(lancerButton);
        bottomPanel.add(arreterButton);

        // Assembler tous les composants
        mainPanel.add(visualiseurPanel, BorderLayout.NORTH);
        mainPanel.add(scrollPane, BorderLayout.CENTER);
        mainPanel.add(scoreLabel, BorderLayout.SOUTH);

        panel.add(topBar, BorderLayout.NORTH);
        panel.add(mainPanel, BorderLayout.CENTER);
        panel.add(bottomPanel, BorderLayout.SOUTH);
    }

    /**
     * Crée un bouton rond stylisé
     */
    private JButton creerBoutonRond(String texte) {
        JButton bouton = new JButton(texte);
        bouton.setFont(new Font("Arial", Font.BOLD, 16));
        bouton.setForeground(Jeu.COULEUR_TEXTE);
        bouton.setBackground(Jeu.COULEUR_SECONDAIRE);
        bouton.setFocusPainted(false);
        bouton.setBorderPainted(false);
        bouton.setCursor(new Cursor(Cursor.HAND_CURSOR));
        bouton.setPreferredSize(new Dimension(120, 50));

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
        visualiseurPanel.repaint();
    }

    /**
     * Lance le karaoké
     */
    public void lancerKaraoke() {
        enCours = true;
        score = 0;
        afficherScore(0);

        // Simulation de paroles qui défilent
        String[] paroles = {
                "Premier couplet - Ligne 1",
                "Premier couplet - Ligne 2",
                "Premier couplet - Ligne 3",
                "Premier couplet - Ligne 4",
                "Refrain - Ligne 1",
                "Refrain - Ligne 2",
                "Refrain - Ligne 3",
                "Deuxième couplet - Ligne 1",
                "Deuxième couplet - Ligne 2",
                "Deuxième couplet - Ligne 3"
        };

        // Arrêter le timer précédent s'il existe
        if (timer != null) {
            timer.cancel();
        }

        // Créer un nouveau timer pour faire défiler les paroles
        timer = new Timer();
        final int[] index = {0};
        timer.scheduleAtFixedRate(new TimerTask() {
            @Override
            public void run() {
                if (index[0] < paroles.length) {
                    final String parolesActuelles = paroles[index[0]];
                    SwingUtilities.invokeLater(() -> {
                        afficherParoles(parolesActuelles);
                        visualiseurPanel.repaint();
                        // Simuler un score qui augmente
                        afficherScore(score + (int)(Math.random() * 50));
                    });
                    index[0]++;
                } else {
                    // Fin des paroles
                    cancel();
                    SwingUtilities.invokeLater(() -> {
                        afficherParoles("Fin de la chanson!");
                        JOptionPane.showMessageDialog(panel,
                                "Votre score final: " + score,
                                "Félicitations!",
                                JOptionPane.INFORMATION_MESSAGE);
                        enCours = false;
                    });
                }
            }
        }, 0, 2000); // Défiler toutes les 2 secondes
    }

    /**
     * Arrête le karaoké
     */
    public void arreterKaraoke() {
        enCours = false;
        if (timer != null) {
            timer.cancel();
        }
        afficherParoles("Karaoké arrêté");
        visualiseurPanel.repaint();
    }

    /**
     * Affiche l'interface du karaoké
     */
    public void afficherInterfaceKaraoke() {
        // Déjà implémenté dans initialiserPanel()
    }

    /**
     * Affiche le score actuel
     * @param score Score à afficher
     */
    public void afficherScore(int score) {
        this.score = score;
        scoreLabel.setText("Score: " + score);
    }

    /**
     * Affiche les paroles de la chanson
     * @param paroles Paroles à afficher
     */
    public void afficherParoles(String paroles) {
        parolesArea.setText(paroles);
    }
}
