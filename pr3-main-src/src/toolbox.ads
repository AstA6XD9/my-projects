-- Module contenant des types et des sous programmes utiles pour les
-- différents programmes du projets

package ToolBox is

    --Définision de types utiles pour le projet

    --Type pour représenter les octets
    type T_Octet is mod 2 ** 8;	-- sur 8 bits
    for T_Octet'Size use 8;

    -- Type représentant un bit (0 ou 1).
    type T_Bit is mod 2;
    for T_Bit'Size use 2;

    -- Structure pour manipuler un tableau d'entiers avec une taille variable.
    -- Sert a representer les differents caracters possibles, on n'utilise
    -- pas de tableau d'octet car le caractere de fin de fichier a pour code -1
    type T_entiers is array(0..256) of Integer;

    type T_Tableau_entier is
        record
            Elements: T_entiers ;
            Taille: Integer;
        end record;

    -- Structure pour manipuler un tableau de bits avec une taille variable.
    -- Sert a stocker des codes binaires
    type T_bits is array(0..256) of T_bit;

    type T_Tableau_bit is
        record
            Elements: T_bits;
            Taille: Integer;
        end record;

    -- Type représentant un symbole avec son code binaire de Huffman associé.
    type T_symbole is
        record
            Symbole: Integer;
            Code: T_Tableau_bit;
        end record;

    -- Structure pour manipuler un tableau de symboles avec une taille.
    type T_Symboles is array(0..256) of T_symbole;

    type T_Tableau_Symbole is
        record
            Elements: T_Symboles;
            Taille: Integer;
        end record;

    --Procedures d'affichage

    -- Affiche un tableau de bits.
    -- Utilisée dans le mode bavard des fichiers "compresser" et "decompresser".
    procedure Afficher_Tab_Bit(Tab : in T_Tableau_bit);

    -- Affiche un tableau d'entiers.
    -- Utilisée pour le débogage uniquement (ne fait pas partie du programme final).
    procedure Afficher_Tab_int(Tab : in T_Tableau_entier);

    -- Affiche un tableau de d'entiers en traduisant les entiers pour leur symbole associé en ASCII.
    -- Utilisée pour le débogage uniquement (ne fait pas partie du programme final).
    procedure Afficher_Tab_symboles(Tab : in T_Tableau_entier);

    -- Affiche un caractère donné en fonction de son code.
    procedure Afficher_Carac (Code: in Integer);

    -- Convertit un octet en tableau de bits.
    function To_Binaire(symbole : in T_Octet ) return T_Tableau_bit;

end ToolBox;
