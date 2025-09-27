with Ada.IO_Exceptions;
with Ada.Command_line;		use Ada.Command_line;
with Arbre_Huffman;         use Arbre_Huffman;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with ToolBox;               use ToolBox;


procedure Decompresser is

    --Procédure pour afficher le codage des symboles du fichier original
    --Ex : [0,1,0] -> "e"
    procedure Afficher_Symboles_inv(Tab : in T_Tableau_Symbole) is
    begin
        for i in 0..Tab.Taille-1 loop
            Afficher_Tab_bit(Tab.Elements(i).Code);
            Put("  --> ");
            Afficher_Carac(Tab.Elements(i).Symbole);
            Put_Line("");
        end loop;
    end Afficher_Symboles_inv;

    --Procédure pour remplir une chaine de bits
    procedure remplir_chaine(Stream: Stream_Access; chaine: in out T_Tableau_bit) is
        Octet : T_Octet;
    begin
        if chaine.Taille = 0 then
            Octet := T_Octet'Input(Stream);
            chaine := To_Binaire(Octet);
        end if;
    end remplir_chaine;


    --Procédure pour enlever le premier élément d'une chaine de bits
    procedure enlever_carac(chaine : in out T_Tableau_bit) is
    begin
        chaine.Taille := chaine.Taille - 1;
        for I in 0..chaine.Taille-1 loop
            chaine.Elements(I) := chaine.Elements(I+1);
        end loop;
    end enlever_carac;


    --Fonction pour reconstruire un tableau de symboles à partir du fichier compréssé
    function Reconstruire_Symbole (Stream: Stream_Access) return T_Tableau_entier is
        symboles : T_Tableau_entier;
        Octet : T_Octet;
        Position_Fin : Integer;
        Fin_Tab : Boolean;
        Indice_inv : Integer;
    begin

        Position_Fin := Integer(T_Octet'Input(Stream));
        symboles.Taille := 0;
        Fin_Tab := False;

        loop
            Octet := T_Octet'Input(Stream);
            if symboles.Taille > 0 and then Integer(Octet) = symboles.Elements(symboles.Taille-1) then
                Fin_Tab := True;
            else
                symboles.Elements(symboles.Taille)  :=  Integer(Octet);
                symboles.Taille := symboles.Taille + 1;
            end if;
        exit when Fin_Tab;
        end loop;

        --Insertion de fin de fichier au bon endroit
        symboles.Taille := symboles.Taille + 1;
        for I in 0..(symboles.Taille - Position_Fin - 1) loop
            Indice_inv := symboles.Taille - I;-- on compte de la fin du tableau jusqu'a la position de l'indice de fin
            symboles.Elements(Indice_inv) := symboles.Elements(Indice_inv-1);
        end loop;
        symboles.Elements(Position_Fin) := -1;
        return symboles;
    end Reconstruire_Symbole;


    --Procédure pour reconstruire la structure de l'arbre (seul les fréquences changent mais elles n'ont aucune importances pour la décompression du code)
   procedure Reconstruire_Arbre (Stream : Stream_Access; symboles: T_Tableau_entier; Arbre : out T_Arbre; chaine : out T_Tableau_bit) is
        Symbole_Code : Integer;
        Symbole_Octet : T_Octet;
        Nb_feuille : Integer;

        --Sous-procédure récursive pour construire l'arbre
        procedure Construction_Recc (Stream : Stream_Access; Arbre : in out T_Arbre) is
            SAG, SAD : T_Arbre;

        begin
            if Nb_feuille < symboles.Taille then
                remplir_chaine(Stream, chaine);
                if chaine.Elements(0) = 0 then --Noeud interne
                    enlever_carac(chaine);
                    Initialiser_Vide(SAG);
                    Initialiser_Vide(SAD);
                    Construction_Recc(Stream,SAG);
                    Construction_Recc(Stream,SAD);
                    Arbre := Fusionner(SAG,SAD);
                else
                    enlever_carac(chaine);
                    Initialiser_Feuille(Arbre,symboles.Elements(Nb_feuille));
                    Nb_feuille := Nb_feuille + 1;
                end if;
            end if;
        end Construction_Recc;
    begin
        chaine.Taille := 0;
        Initialiser_Vide(Arbre);
        Nb_feuille := 0;
        Construction_Recc(Stream, Arbre);

   end Reconstruire_Arbre;

    --Procédure pour lire traduire les données décompressées
    procedure Lire_Donnees_Compression(Input_Stream : Stream_Access;Output_Stream : Stream_Access;arbre: in T_Arbre ; chaine: in out T_Tableau_bit) is
        Noeud_Courant: T_Arbre; --on peut mettre arbre en in out ou fr une fonction
        Carac : Integer;
    begin
        loop
            Noeud_Courant:= arbre;
            loop
                remplir_chaine(Input_Stream,chaine);
                if chaine.Elements(0) = 0 then
                    Noeud_Courant := Arbre_Gauche(Noeud_Courant);--Aller à gauche
                else
                    Noeud_Courant := Arbre_Droit(Noeud_Courant);--Aller à droite
                end if;
                enlever_carac(chaine);
                exit when Est_Feuille(Noeud_Courant); --Arrét sur une feuille
            end loop;
            Carac := Le_Symbole(Noeud_Courant);
            Noeud_Courant := arbre;
            if Carac /= -1 then
                T_Octet'Write(Output_Stream,T_Octet(Carac));--Ecriture du caractère
            end if;
            exit when Carac = -1;
        end loop;

    end Lire_Donnees_Compression;

    --Fonction pour vérifier l'extension du fichier
    function Ends_With_hff(S : String) return Boolean is
        Hff : String(1..4) := ".hff";  -- Extension attendue
    begin
        -- Vérifie que la chaine est plus grande que la taille de .hff
        if Hff'Length > S'Length then
            return False;
        end if;

        -- Compare la fin de la chaîne S avec ".hff"
        return S(S'Length - Hff'Length + 1 .. S'Length) = Hff;
    end Ends_With_hff;

    --Variables pour la gestion des fichiers et des données
    Fichier_Entree : Ada.Streams.Stream_IO.File_Type;
    Stream_Entree         : Stream_Access;
    Fichier_Sortie : Ada.Streams.Stream_IO.File_Type;
    Stream_Sortie         : Stream_Access;
    Nom_Fichier1 : Unbounded_String;
    Nom_Fichier2 : Unbounded_String;

    Arbre : T_Arbre;
    Octet : T_Octet;
    chaine : T_Tableau_bit;
    symboles : T_Tableau_entier;
    Option : Unbounded_String;

    --Exceptions spécifiques
    No_Argument_Error : Exception;
    Option_Argument_Error : Exception;
    Extension_Error : Exception;


begin
    --appel en ligne de comande
    if Argument_Count < 1 then
        raise No_Argument_Error;
    elsif Argument_Count = 1 then
        Nom_Fichier1 := To_Unbounded_String(Argument(1));
        Option := To_Unbounded_String("-b");
    else
        Option := To_Unbounded_String(Argument(Argument_Count-1));
    end if;
    --vérification de l'extension du fichier
    if Ends_With_hff(Argument(Argument_Count)) then
        Nom_Fichier1 := To_Unbounded_String(Argument(Argument_Count));
        Nom_Fichier2 := To_Unbounded_String(To_String(Nom_Fichier1) & ".d");
    else
        raise Extension_Error;
    end if;

    --Gestion des fichiers
    Open(Fichier_Entree, In_File,To_String(Nom_Fichier1));
    Stream_Entree := Stream(Fichier_Entree);

    Create(Fichier_Sortie, Out_File, To_String(Nom_Fichier2));
    Stream_Sortie := Stream(Fichier_Sortie);

    --Reconstruction du tableau des symboles, ex: [120,256,-1,12]
    symboles := Reconstruire_Symbole(Stream_Entree);

    --Reconstruction de l'arbre de Huffman, en s'intéressant uniquement a la structure et aux symboles
    Reconstruire_Arbre(Stream_Entree,symboles, Arbre,chaine);
    if To_String(Option) = "-b" or To_String(Option) = "--bavard" then
        Afficher(Arbre);
        Afficher_Symboles_inv(Creer_Tableau_Symbole(Arbre));
    elsif To_String(Option) = "-s" or To_String(Option) ="--silencieux" then
        Null;
    else
        raise Option_Argument_Error;
    end if;

    --Ecrire la traduction du code simplifié en binaire classique
    Lire_Donnees_Compression(Stream_Entree, Stream_Sortie, Arbre, chaine);

    --Fermeture des fichiers
    Close(Fichier_Entree);
    Close(Fichier_Sortie);

exception
        --Gestion des exceptions
    when No_Argument_Error =>
        Put_Line ("Pas de fichier.");
        New_Line;
        Put_Line ("Usage : " & Command_Name & " <fichier>");

    when Option_Argument_Error =>
        Put_Line("L'option donnée n'existe pas :" & Argument(Argument_Count-1));

    when Ada.IO_Exceptions.Name_Error =>
        Put_Line ("Fichier inexisant : " & Argument(Argument_Count));

    when Extension_Error =>
        Put_Line("Mauvaise extension pour le fichier :" & Argument(Argument_Count));
        New_Line;
        Put_Line("Ce programme décompresse seulement les fichiers .hff");

end Decompresser;
