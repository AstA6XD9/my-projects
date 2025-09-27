import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

/**
 * Page de choix de musique
 */
public class PageChoixMusique implements Page {
    private Jeu jeu;
    private JPanel panel;
    private JList<String> listMusiques;
    private DefaultListModel<String> listModel;

    /**
     * Constructeur de la page de choix de musique
     * @param jeu Instance du jeu
     */
    public PageChoixMusique(Jeu jeu) {
        this.jeu = jeu;
        panel = new JPanel();
        initialiserPanel();
    }

    /**
     * Initialise le panneau de la page
     */
    private void initialiserPanel() {
        panel.setLayout(new BorderLayout());
        panel.setBackground(Jeu.COULEUR_FOND);

        // Barre supérieure avec titre
        JPanel topBar = new JPanel(new BorderLayout());
        topBar.setBackground(new Color(24, 24, 24));
        topBar.setBorder(new EmptyBorder(15, 20, 15, 20));

        JLabel titre = new JLabel("Bibliothèque Musicale", SwingConstants.CENTER);
        titre.setFont(new Font("Arial", Font.BOLD, 28));
        titre.setForeground(Jeu.COULEUR_ACCENT);
        topBar.add(titre, BorderLayout.CENTER);

        // Panneau principal
        JPanel mainPanel = new JPanel(new BorderLayout(20, 20));
        mainPanel.setBackground(Jeu.COULEUR_FOND);
        mainPanel.setBorder(new EmptyBorder(20, 20, 20, 20));

        // Panel de la liste de chansons
        JPanel listPanel = new JPanel(new BorderLayout());
        listPanel.setBackground(Jeu.COULEUR_FOND);

        // Ajout d'un en-tête au panneau
        JLabel listHeader = new JLabel("  Titre                                      Artiste                                     Durée");
        listHeader.setFont(new Font("Arial", Font.BOLD, 14));
        listHeader.setForeground(new Color(180, 180, 180));
        listHeader.setBorder(new EmptyBorder(0, 10, 5, 0));
        listPanel.add(listHeader, BorderLayout.NORTH);

        // Création d'un modèle personnalisé pour la liste
        listModel = new DefaultListModel<>();
        listModel.addElement("Chanson 1 - Artiste A - 3:45");
        listModel.addElement("Chanson 2 - Artiste B - 4:12");
        listModel.addElement("Chanson 3 - Artiste C - 3:30");
        listModel.addElement("Chanson 4 - Artiste D - 2:55");
        listModel.addElement("Chanson 5 - Artiste E - 4:05");
        listModel.addElement("Chanson 6 - Artiste F - 3:22");
        listModel.addElement("Chanson 7 - Artiste G - 5:15");
        listModel.addElement("Chanson 8 - Artiste H - 3:48");
        listModel.addElement("Chanson 9 - Artiste I - 4:33");
        listModel.addElement("Chanson 10 - Artiste J - 3:10");

        // Création de la liste avec le modèle
        listMusiques = new JList<>(listModel);
        listMusiques.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        listMusiques.setBackground(Jeu.COULEUR_SECONDAIRE);
        listMusiques.setForeground(Jeu.COULEUR_TEXTE);
        listMusiques.setFont(new Font("Arial", Font.PLAIN, 16));
        listMusiques.setFixedCellHeight(40);
        listMusiques.setCellRenderer(new MusiqueListCellRenderer());

        // Ajouter un double-clic pour sélectionner
        listMusiques.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (e.getClickCount() == 2) {
                    selectionnerMusique();
                }
            }
        });

        JScrollPane scrollPane = new JScrollPane(listMusiques);
        scrollPane.setBorder(BorderFactory.createEmptyBorder());
        scrollPane.getVerticalScrollBar().setUnitIncrement(16);
        listPanel.add(scrollPane, BorderLayout.CENTER);

        // Panel pour les boutons
        JPanel boutonsPanel = new JPanel(new FlowLayout(FlowLayout.CENTER, 20, 10));
        boutonsPanel.setBackground(new Color(24, 24, 24));
        boutonsPanel.setBorder(new EmptyBorder(15, 0, 15, 0));

        JButton selectionnerButton = creerBoutonStylise("Sélectionner");
        JButton ecouterButton = creerBoutonStylise("Écouter");
        JButton retourButton = creerBoutonStylise("Retour");

        selectionnerButton.addActionListener(e -> selectionnerMusique());
        ecouterButton.addActionListener(e -> ecouterMusique());
        retourButton.addActionListener(e -> jeu.changerPage(jeu.pageDemarrage));

        boutonsPanel.add(selectionnerButton);
        boutonsPanel.add(ecouterButton);
        boutonsPanel.add(retourButton);

        // Ajout des composants au panneau principal
        mainPanel.add(listPanel, BorderLayout.CENTER);

        // Assemblage final
        panel.add(topBar, BorderLayout.NORTH);
        panel.add(mainPanel, BorderLayout.CENTER);
        panel.add(boutonsPanel, BorderLayout.SOUTH);
    }

    /**
     * Renderer personnalisé pour les cellules de la liste de musiques
     */
    private class MusiqueListCellRenderer extends DefaultListCellRenderer {
        @Override
        public Component getListCellRendererComponent(JList<?> list, Object value, int index, boolean isSelected, boolean cellHasFocus) {
            JLabel label = (JLabel) super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);

            // Styling de base
            label.setBorder(new EmptyBorder(5, 10, 5, 10));

            // Styling pour les éléments sélectionnés
            if (isSelected) {
                label.setBackground(Jeu.COULEUR_ACCENT);
                label.setForeground(Color.WHITE);
            } else {
                // Alternance des couleurs pour les lignes
                if (index % 2 == 0) {
                    label.setBackground(new Color(40, 40, 40));
                } else {
                    label.setBackground(new Color(50, 50, 50));
                }
                label.setForeground(Jeu.COULEUR_TEXTE);
            }

            return label;
        }
    }

    /**
     * Crée un bouton stylisé façon Spotify
     */
    private JButton creerBoutonStylise(String texte) {
        JButton bouton = new JButton(texte);
        bouton.setFont(new Font("Arial", Font.BOLD, 14));
        bouton.setForeground(Jeu.COULEUR_TEXTE);
        bouton.setBackground(Jeu.COULEUR_SECONDAIRE);
        bouton.setFocusPainted(false);
        bouton.setBorderPainted(false);
        bouton.setCursor(new Cursor(Cursor.HAND_CURSOR));
        bouton.setPreferredSize(new Dimension(120, 40));

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
     * Liste les musiques disponibles
     */
    public void listerMusiques() {
        // Déjà implémenté dans initialiserPanel()
    }

    /**
     * Sélectionne une musique
     */
    private void selectionnerMusique() {
        String musique = listMusiques.getSelectedValue();
        if (musique != null) {
            selectionnerMusique(musique);
        } else {
            JOptionPane.showMessageDialog(panel,
                    "Veuillez sélectionner une musique",
                    "Sélection requise",
                    JOptionPane.WARNING_MESSAGE);
        }
    }

    /**
     * Sélectionne une musique
     * @param nom Nom de la musique
     */
    public void selectionnerMusique(String nom) {
        JOptionPane.showMessageDialog(panel,
                "Musique sélectionnée : " + nom,
                "Sélection",
                JOptionPane.INFORMATION_MESSAGE);
        jeu.changerPage(jeu.pageJeuKaraoke);
    }

    /**
     * Écoute une musique
     */
    private void ecouterMusique() {
        String musique = listMusiques.getSelectedValue();
        if (musique != null) {
            afficherPreEcoute(musique);
        } else {
            JOptionPane.showMessageDialog(panel,
                    "Veuillez sélectionner une musique",
                    "Sélection requise",
                    JOptionPane.WARNING_MESSAGE);
        }
    }

    /**
     * Affiche la pré-écoute d'une musique
     * @param nom Nom de la musique
     */
    /**
     * Affiche la pré-écoute d'une musique
     * @param nom Nom de la musique
     */
    public void afficherPreEcoute(String nom) {
        // Création du panel pour la pré-écoute
        JPanel preEcoutePanel = new JPanel(new BorderLayout());
        preEcoutePanel.setBackground(Jeu.COULEUR_SECONDAIRE);
        preEcoutePanel.setBorder(new EmptyBorder(15, 15, 15, 15));

        // Titre de la musique
        JLabel titreLabel = new JLabel("Pré-écoute: " + nom);
        titreLabel.setFont(new Font("Arial", Font.BOLD, 16));
        titreLabel.setForeground(Jeu.COULEUR_TEXTE);
        titreLabel.setBorder(new EmptyBorder(0, 0, 10, 0));

        // Barre de progression
        JProgressBar progressBar = new JProgressBar(0, 100);
        progressBar.setValue(0);
        progressBar.setStringPainted(true);
        progressBar.setBackground(Jeu.COULEUR_FOND);
        progressBar.setForeground(Jeu.COULEUR_ACCENT);

        // Bouton pour fermer
        JButton fermerButton = new JButton("Fermer");
        fermerButton.setFont(new Font("Arial", Font.PLAIN, 14));

        // Ajout des composants au panel
        preEcoutePanel.add(titreLabel, BorderLayout.NORTH);
        preEcoutePanel.add(progressBar, BorderLayout.CENTER);
        preEcoutePanel.add(fermerButton, BorderLayout.SOUTH);

        // Création de la boîte de dialogue
        JDialog preEcouteDialog = new JDialog();
        preEcouteDialog.setTitle("Écoute de " + nom);
        preEcouteDialog.setModal(false);
        preEcouteDialog.setSize(400, 150);
        preEcouteDialog.setLocationRelativeTo(panel);
        preEcouteDialog.setContentPane(preEcoutePanel);

        // Gestion de l'événement pour fermer la boîte de dialogue
        fermerButton.addActionListener(e -> preEcouteDialog.dispose());

        // Affichage de la boîte de dialogue
        preEcouteDialog.setVisible(true);

        // Simulation de progression
        java.util.Timer timer = new java.util.Timer();
        final int[] progress = {0};

        timer.scheduleAtFixedRate(new java.util.TimerTask() {
            @Override
            public void run() {
                if (progress[0] <= 100) {
                    SwingUtilities.invokeLater(() -> progressBar.setValue(progress[0]));
                    progress[0] += 5;
                } else {
                    this.cancel();
                    SwingUtilities.invokeLater(() -> preEcouteDialog.dispose());
                }
            }
        }, 0, 200);
    }
}