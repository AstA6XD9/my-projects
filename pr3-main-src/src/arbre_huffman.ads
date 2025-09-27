-- Un arbre de Huffman est un arbre binaire dans lequel chaque noeud contient 
-- une fréquence et éventuellement un code si il s'agit d'une feuille.
-- Tous les noeuds possèdent un fils gauche et droit : c'est un arbre binaire parfait.
with ToolBox; use ToolBox;

package Arbre_Huffman is
    
    --Definition de types utilisés dans ce module

    -- Type abstrait représentant un arbre de Huffman.
    type T_Arbre is private;
    
    -- Procédures pour initialiser et détruire un arbre de Huffman
    
	-- Initialise un arbre de Huffman vide.
	procedure Initialiser_Vide(Arbre: out T_Arbre) with
            Post => Est_Vide (Arbre);
    
    -- Initialise une feuille de l'arbre de Huffman avec un symbole donné et une fréquence initiale de 1.
    -- (On initialise une feuille quand on recontre le symbole pour la premiere fois -> fréquence de 1)
    procedure Initialiser_Feuille(Arbre: out T_Arbre; symbole: in Integer) ;
    
    -- Initialise une feuille de l'arbre de Huffman pour le symbole de fin de fichier.
    -- La frequence du symbole de fin est conventionellement mis à 0.
    procedure Initialiser_Fin_Fichier(Arbre: out T_Arbre) ;
            

	-- Détruit un arbre de Huffman et libère la mémoire associée.
	procedure Detruire (Arbre : in out T_Arbre);

    --Prédicats sur les arbres de Huffman

	-- Vérifie si un arbre de Huffman est vide.
    function Est_Vide (Arbre : in T_Arbre) return Boolean;
    
    -- Vérifie si un arbre de Huffman est une feuille.
    function Est_Feuille (Arbre : in T_Arbre) return Boolean;
    
    -- Fusionne deux arbres de Huffman en un seul en créant un noeud parent.
    -- La fréquence du noeud parent est la somme des frequences des deux noeuds.
    function Fusionner (SousArbreGauche: in T_Arbre; SousArbreDroit: in T_Arbre) return T_Arbre ;
    
    --Les différents accesseurs : 
    
    -- Renvoie la fréquence de la racine de l'arbre de Huffman.
    function La_Frequence (Arbre : in T_Arbre) return Integer ;
            
    -- Renvoie le code binaire associé à la racine de l'arbre de Huffman.
    function Le_Code (Arbre : in T_Arbre) return T_Tableau_bit;

    -- Renvoie le symbole de la racine d'un Arbre de Huffman
    function Le_Symbole (Arbre : in T_Arbre ) return Integer;
    
    -- Renvoie le sous-arbre gauche d'un arbre de Huffman.
    function Arbre_Gauche (Arbre : in T_Arbre) return T_Arbre;
    
    -- Renvoie le sous-arbre droit d'un arbre de Huffman.
    function Arbre_Droit (Arbre : in T_Arbre) return T_Arbre;
    
    --Fonctions pour mettre à jour un arbre de Huffman: 
    
    -- Augmente la fréquence de la racine de l'arbre de Huffman de 1.
    procedure Augmenter_Frequence (Arbre: in out T_Arbre);
    
    -- Met à jour les codes binaires dans l'arbre de Huffman en fonction de la structure de l'arbre.
    procedure Mettre_A_Jour_Code(Arbre: in out T_arbre);
    
    -- Procédures d'affichage:
    
    -- Affiche la structure de l'arbre de Huffman.
    procedure Afficher (Arbre : in T_Arbre);
    
    -- Crée un tableau de symboles à partir de l'arbre de Huffman.
    function Creer_Tableau_Symbole(Arbre: in out T_Arbre) return T_Tableau_Symbole; -- return T_Tableau_Symbole
    
private
 
    -- Type interne représentant un noeud de l'arbre de Huffman.
    type T_Noeud;
    
    -- Type pointeur pour manipuler les noeuds de l'arbre.
    -- Un objet arbre de huffman est donc un pointeur vers un noeud
	type T_Arbre is access T_Noeud;

    -- Structure représentant un noeud dans l'arbre de Huffman.
	type T_Noeud is
        record
            Frequence: Integer; -- nombre de fois que les caractères du noeud ou de ses fils apparaissent
            Code: T_Tableau_bit; -- pour les feuilles: code de Huffman du caractère
            SAG: T_arbre;
            SAD: T_arbre;
            Symbole : integer; 
        end record;

end Arbre_Huffman;
