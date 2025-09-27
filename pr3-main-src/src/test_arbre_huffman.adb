with Ada.Text_IO;            use Ada.Text_IO;
with Arbre_Huffman;

procedure Test_Arbre_Huffman is

    use Arbre_Huffman;

    Arbre1, Arbre2,Arbre3 ArbreFusion, ArbreFusion2: T_Arbre;
    Tableau_Symboles: T_Tableau_Symbole;

begin
    -- Test Initialisation Vide
    Initialiser_Vide(Arbre1);
    pragma Assert(Est_Vide(Arbre1));

    -- Test Initialisation Feuille
    Initialiser_Feuille(Arbre1, Symbole => 65); -- Symbole 'A'
    pragma Assert(not Est_Vide(Arbre1));
    pragma Assert(Est_Feuille(Arbre1));
    pragma Assert(Le_Symbole(Arbre1) = 65);

    -- Test Initialisation Fin de Fichier
    Initialiser_Fin_Fichier(Arbre2);
    pragma Assert(Le_Symbole(Arbre2) = -1);

    -- Test Fusion
    ArbreFusion := Fusionner(Arbre1, Arbre2);
    pragma Assert(La_Frequence(ArbreFusion) = 1 + 0);
    pragma Assert(Arbre_Gauche(ArbreFusion) = Arbre1);
    pragma Assert(Arbre_Droit(ArbreFusion) = Arbre2);

    Initialiser_Feuille(Arbre3, Symbole => 10); -- Symbole '/n'
    ArbreFusion2 := Fusionner(ArbreFusion, Arbre3);
    pragma Assert(La_Frequence(ArbreFusion2) = 1 + 1);

    -- Test Augmenter Fréquence
    Augmenter_Frequence(Arbre1);
    pragma Assert(La_Frequence(Arbre1) = 2);

    -- Test Création Tableau de Symboles
    Tableau_Symboles := Creer_Tableau_Symbole(ArbreFusion);
    pragma Assert(Tableau_Symboles.Taille = 2);
    pragma Assert(Tableau_Symboles.Elements(0).Symbole = 65);
    pragma Assert(Tableau_Symboles.Elements(1).Symbole = -1);

    -- Nettoyage
    Detruire(Arbre1);
    Detruire(Arbre2);
    Detruire(ArbreFusion);

    Put_Line("Tous les tests sont passés avec succès.");
end Test_Arbre_Huffman;
