

generic
    type T_Cle is private;
    type T_Valeur is private;
    taille_tableau : integer ;
    with function Fonction_Hachage (Cle : T_Cle) return Natural;

package TH is



    type T_TH is limited private;

    -- Initialiser une TH vide.
    procedure Initialiser (Sda : out T_TH) with
            Post => Est_Vide(Sda);

    -- Détruire une TH.
    procedure Detruire (Sda : in out T_TH);

    -- Vérifier si une TH est vide.
    function Est_Vide (Sda : in T_TH) return Boolean;

    -- Obtenir le nombre d'éléments de la TH.
    function Taille (Sda : in T_TH) return Integer with
            Post => Taille'Result >= 0;

    -- Enregistrer une valeur associée à une clé dans la TH.
    procedure Enregistrer (Sda : in out T_TH; Cle : in T_Cle; Valeur : in T_Valeur);

    -- Supprimer la valeur associée à une clé dans la TH.
    -- Lève une exception si la clé est absente.
    procedure Supprimer (Sda : in out T_TH; Cle : in T_Cle);

    -- Vérifier si une clé est présente dans la TH.
    function Cle_Presente (Sda : in T_TH; Cle : in T_Cle) return Boolean;

    -- Obtenir la valeur associée à une clé dans la TH.
    -- Lève une exception si la clé est absente.
    function La_Valeur (Sda : in T_TH; Cle : in T_Cle) return T_Valeur;

    -- Appliquer un traitement pour chaque clé/valeur dans la TH.
    generic
        with procedure Traiter (Cle : in T_Cle; Valeur : in T_Valeur);
    procedure Pour_Chaque (Sda : in T_TH);

    -- Afficher la structure interne de la TH.
    generic
        with procedure Afficher_Cle (Cle : in T_Cle);
        with procedure Afficher_Donnee (Valeur : in T_Valeur);
    procedure Afficher_Debug (Sda : in T_TH);

private
    type T_Cellule ;
    type T_LCA is access all T_Cellule;
    type T_Cellule is record
        Cle : T_Cle;
        Valeur : T_Valeur ;
        Suivant : T_LCA ;
    end record ;

    type T_Tableau is array (Positive range <>) of T_LCA;
    type T_TH is record
        Tableau : T_Tableau(1 .. taille_tableau);
        Nombre_Elem : Natural := 0;
    end record;

end TH;
