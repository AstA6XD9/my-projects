with Ada.IO_Exceptions;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_line;		use Ada.Command_line;
with Arbre_Huffman;         use Arbre_Huffman;
with ToolBox;               use ToolBox;
-- Ce programme compresse un fichier texte en utilisant l'algorithme de compression de Huffman.
-- Il prend un fichier en entrée, génère un fichier compressé avec une structure de codage Huffman, et écrit la compression.
procedure Compresser is

    --Structure pour stocker un tableau d'arbres d Huffman
    type T_elts is array(0..256) of T_Arbre;
    type T_Tableau_arbres is
        record
            Elements: T_elts;
            Taille: Integer;
        end record ;

    --Affiche les symboles et leurs codes
    procedure Afficher_Symboles(Tab : in T_Tableau_Symbole) is
    begin
        for i in 0..Tab.Taille-1 loop
            Afficher_Carac(Tab.Elements(i).Symbole);
            Put("  --> ");
            Afficher_Tab_bit(Tab.Elements(i).Code);
            Put_Line("");
        end loop;
    end Afficher_Symboles;

    --***Fonctionnalités principales ***

    --Trie un tableau d'arbre en fonction de leurs fréquences
    procedure Trier(Tableau : in out T_Tableau_arbres) is
        Temp : T_Arbre;
        Freq_Mini : Integer;
        Indice_Mini : Integer;
    begin
        for I in 0..Tableau.Taille-1 loop
            Freq_Mini := La_Frequence(Tableau.Elements(I));
            Indice_Mini := I;
            for J in I+1..Tableau.Taille-1 loop
                if La_Frequence(Tableau.Elements(J)) < Freq_Mini then
                    Freq_Mini := La_Frequence(Tableau.Elements(J));
                    Indice_Mini := J;
                end if ;
            end loop ;
            temp := Tableau.Elements(I);
            Tableau.Elements(I) := Tableau.Elements(Indice_Mini) ;
            Tableau.Elements(Indice_Mini):=temp;
        end loop ;
    end Trier ;

    --convertit un octet en tableau de bits (binaire)
    function entier_binaire(symbole : in T_Octet ) return T_Tableau_bit is
        Tableau : T_Tableau_bit ;
        Octet : T_Octet;
    begin
        Octet := symbole;
        Tableau.Taille := 8;
        for I in 0..7 loop
            Tableau.Elements(I) := T_bit(Octet / 128);
            Octet := Octet * 2;
        end loop;

        return Tableau;
    end entier_binaire ;

    --Creer la suite de bit correspondant aux symboles
    function Creer_Liste_Symbole(Symboles: in T_Tableau_Symbole) return T_Tableau_entier is
        Tab : T_Tableau_entier;
        Indice_Fin : Integer;
        Indice_Decroissant : Integer;
    begin
        Tab.Taille := Symboles.Taille + 1;
        for I in 0..Symboles.Taille-1 loop
            Tab.Elements(I) := Symboles.Elements(I).Symbole;
            if Symboles.Elements(I).Symbole = -1 then
                Indice_Fin := I;
            end if;
        end loop;
        for J in 0..Indice_Fin-1 loop
            Indice_Decroissant := Indice_Fin-J;
            Tab.Elements(Indice_Decroissant) := Tab.Elements(Indice_Decroissant-1);
        end loop;
        Tab.Elements(0) := Indice_Fin;
        Tab.Elements(Symboles.Taille) := Tab.Elements(Symboles.Taille-1);--on double le dernier élément
        return Tab;

    end Creer_Liste_Symbole;

    --Ecrit un octet present sous forme de bit dans le fichier
    procedure Ecrire_Octet( Output_Stream : Stream_Access; Chaine : in out T_Tableau_bit) is

        Octet:T_Octet;

    begin
        Octet := 0;
        for I in 0..7 loop
            Octet := (Octet*2) or T_Octet(Chaine.Elements(I));
        end loop;
        T_Octet'Write(Output_Stream, Octet);
        for I in 8..Chaine.Taille -1 loop
            Chaine.Elements(I-8) := Chaine.Elements(I);
        end loop;
        Chaine.Taille := Chaine.Taille -8;

    end Ecrire_Octet;

    --ecrire la structure de l'arbre dans le fichier
    function Ecrire_Arbre (Output_Stream: in Stream_Access; Arbre : T_Arbre) return T_Tableau_bit is

        procedure Ecrire_Arbre_rec (Output_Stream: in Stream_Access; Arbre : T_Arbre; Chaine : in out T_Tableau_bit) is
        begin
            if Chaine.Taille > 7 then
                Ecrire_Octet(Output_stream, Chaine);
            end if;
            if Est_Feuille(Arbre) then
                Chaine.Elements(Chaine.Taille) := 1;
                Chaine.Taille := Chaine.Taille + 1;
            else
                Chaine.Elements(Chaine.Taille) := 0;
                Chaine.Taille := Chaine.Taille + 1;
                Ecrire_Arbre_rec(Output_Stream, Arbre_Gauche(Arbre), Chaine);
                Ecrire_Arbre_rec(Output_Stream, Arbre_Droit(Arbre), Chaine);

            end if;

        end Ecrire_Arbre_rec;

        Tab : T_Tableau_bit;

    begin
        Tab.Taille := 0;
        if not(Est_Vide(Arbre)) then
            Ecrire_Arbre_rec(Output_Stream, Arbre, Tab);
        end if;
        return Tab;

    end Ecrire_Arbre;

    --procedure pour reecrire le texte code dans le fichier
    procedure Ecrire_Fichier(Input_File : in out Ada.Streams.Stream_IO.File_Type; Input_Stream: in Stream_Access;Output_stream : in Stream_Access; Chaine: in out T_Tableau_bit;Correspondance: T_Tableau_Symbole; fini :  in Boolean := False) is --T_Tableau_bit

        --Ajoute un symbole a la chaine
        procedure Ajouter_Chaine(Chaine: in out T_Tableau_bit; Symbole : in Integer; Correspondance: in T_Tableau_Symbole) is
            symbole_traduit : T_Tableau_bit;
        begin
            symbole_traduit.Taille := 0;
            for I in 0..Correspondance.Taille loop
                if Correspondance.Elements(I).symbole = Symbole then
                    symbole_traduit :=  Correspondance.Elements(I).code;
                end if;
            end loop;
            for I in 0..symbole_traduit.Taille-1 loop
                Chaine.Elements(Chaine.Taille) := symbole_traduit.Elements(I);
                Chaine.Taille := Chaine.Taille + 1;
            end loop;

        end Ajouter_Chaine;

        --complete la chaine avec le bon nombre de 0 (pour la fin)
        procedure Completer_Chaine(Chaine: in out T_Tableau_bit) is
            nb_zero : Integer;
        begin
            nb_zero := 8 - Chaine.Taille mod 8;
            for i in 1..nb_zero loop
                Chaine.Elements(Chaine.Taille) := 0;
                Chaine.Taille := Chaine.Taille + 1;
            end loop;

            end Completer_Chaine;


        Octet : T_Octet;

    begin
        --ecrire ce qu'il y a dans la chaine
        while Chaine.Taille > 7 loop
            Ecrire_Octet(Output_stream, Chaine);
        end loop;

        --si on a pas fini le processus
        if not(fini) then
            --si on arrive a la fin
            if End_Of_File(Input_File) then
                Ajouter_Chaine(Chaine,-1,Correspondance);
                Completer_Chaine(Chaine);
                Ecrire_Fichier(Input_File, Input_Stream, Output_stream, Chaine, Correspondance,True);
            else
                Octet := T_Octet'Input(Input_Stream);
                Ajouter_Chaine(Chaine,Integer(Octet),Correspondance);
                Ecrire_Fichier(Input_File, Input_Stream, Output_stream, Chaine, Correspondance);
            end if;
        end if;

    end Ecrire_Fichier;

    No_Argument_Error : Exception;
    Option_Argument_Error : Exception;

    --Variables pour la gestion des fichiers et des données
    Input_File : Ada.Streams.Stream_IO.File_Type;
    Output_File : Ada.Streams.Stream_IO.File_Type;
    Input_Stream:Stream_Access;
    Output_Stream : Stream_Access ;
    Tableau : T_Tableau_arbres ;
    Arbre : T_Arbre ;
    Tableau_fusion : T_Tableau_arbres ;
    Symboles : T_Tableau_Symbole ;
    Trouve : Boolean;
    Liste_Symboles : T_Tableau_entier ;
    File_Name1 : Unbounded_String;
    File_Name2 : Unbounded_String;
    Octet : T_Octet;
    Liste_bits : T_Tableau_bit;
    Option : Unbounded_String;


begin

    --Gestion de l'éxécution du programme en ligne de commande
    if Argument_Count < 1 then
        raise No_Argument_Error;
    elsif Argument_Count = 1 then
        File_Name1 := To_Unbounded_String(Argument(1));
        Option := To_Unbounded_String("-b");
    else
        Option := To_Unbounded_String(Argument(Argument_Count-1));
        File_Name1 := To_Unbounded_String(Argument(Argument_Count));
    end if;

    File_Name2 := To_Unbounded_String(To_String(File_Name1) & ".hff");

    Open(Input_File, In_File, To_String(File_Name1));
    Input_Stream := Stream(Input_File);

    --intitialiser le tableau
    Initialiser_Fin_Fichier(Arbre);
    Tableau.Elements(0) := Arbre;
    Tableau.Taille := 1;

    --parcourir le fichier
    while not End_Of_File(Input_File) loop
        Trouve := False;
        Octet := T_Octet'Input(Input_Stream);
        for I in 0..Tableau.Taille-1 loop
            if Le_Symbole(Tableau.Elements(I)) = Integer(Octet) then
                Augmenter_Frequence(Tableau.Elements(I));
                Trouve := True;
            end if;
        end loop;
        if not(Trouve) then
            initialiser_feuille(Arbre,Integer(Octet));
            Tableau.Elements(Tableau.Taille) := Arbre ;
            Tableau.Taille := Tableau.Taille + 1 ;
        end if;
    end loop ;

    -- fusion ;
    while Tableau.Taille >1 loop
        Trier(Tableau) ;
        Tableau_fusion.Taille := Tableau.Taille-1;
        Tableau_fusion.Elements(0):=fusionner(Tableau.Elements(0),Tableau.Elements(1));
        for i in  1..Tableau_fusion.Taille-1 loop
            Tableau_fusion.Elements(i) := Tableau.Elements(i+1);
        end loop;
        Tableau := Tableau_fusion;
    end loop ;

    --ecriture du code
    Arbre := Tableau.Elements(0) ;

    Mettre_A_Jour_Code(Arbre);

    --creation du fichier compressé
    Symboles := Creer_Tableau_Symbole(Arbre);--liste de 2-uplet symboles/codage (T_octet/T_tableau_bit)
    create(Output_File,Out_File,To_String(File_Name2));

    --create(Input_File,In_File,File_Name)
    Output_Stream := Stream(Output_File);
    Input_Stream := Stream(Input_File) ;

    --ajout symboles
    Liste_Symboles := Creer_Liste_Symbole(Symboles);
    put(Liste_Symboles.Taille);--debug
    for I in 0..Liste_Symboles.Taille-1 loop
        T_Octet'Write(Output_Stream,T_Octet(Liste_Symboles.Elements(I)));
    end loop;

    --ajout arbre
    Liste_bits := Ecrire_Arbre(Output_Stream,Arbre);
    Afficher_Tab_Bit(Liste_bits);--debug

    --remettre le curseur du fichier Input_File à 0
    Set_Index(File => Input_File, To => 1);

    --Convertir le texte du fichier
    Ecrire_Fichier(Input_File, Input_Stream, Output_Stream,Liste_bits,Symboles);

    --Gestion de l'affichage
    if To_String(Option) = "-b" or To_String(Option) = "--bavard" then
        Afficher(Arbre);
        Afficher_Symboles(Symboles);
    elsif To_String(Option) = "-s" or To_String(Option) ="--silencieux" then
        Null;
    else
        raise Option_Argument_Error;
    end if;



--Gestion des exceptions
exception
	when No_Argument_Error =>
		Put_Line ("Pas de fichier.");
		New_Line;
        Put_Line ("Usage : " & Command_Name & " <fichier>");

    when Option_Argument_Error =>
        Put_Line("L'option donnée n'existe pas :" & Argument(Argument_Count-1));

	when Ada.IO_Exceptions.Name_Error =>
		Put_Line ("Fichier inexisant : " & Argument(Argument_Count));


end Compresser ;
