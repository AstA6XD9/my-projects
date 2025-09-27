with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;
with Ada.Unchecked_Deallocation;

package body Arbre_Huffman is

    --Sous-programmes internes au module

    procedure Free is
            new Ada.Unchecked_Deallocation (T_Noeud,T_Arbre);

    -- Réalise une copie profonde d'un tableau de bit
    function Deep_Copy (Tableau : in T_Tableau_bit) return T_Tableau_bit is
        Nouveau_Tableau : T_Tableau_bit;
    begin
        Nouveau_Tableau.Taille := Tableau.Taille;
        for i in 0..Tableau.Taille loop
            Nouveau_Tableau.Elements(i) := Tableau.Elements(i);
        end loop;
        return Nouveau_Tableau;
    end Deep_Copy;

    -- Sous-programmes accessibles à l'extérieur du module

	procedure Initialiser_Vide(Arbre: out T_Arbre) is
	begin
		Arbre := null;
    end Initialiser_Vide;

    procedure Initialiser_Feuille(Arbre: out T_Arbre; Symbole: in Integer) is
        Code_Vide : T_Tableau_bit;
    begin
        Arbre := new T_Noeud'(Frequence => 1,SAD => Null, SAG => Null, Symbole => symbole, Code => Code_Vide);
    end Initialiser_Feuille;

    procedure Initialiser_Fin_Fichier(Arbre: out T_Arbre) is
        Code_Vide : T_Tableau_bit;
    begin
        Arbre := new T_Noeud'(Frequence => 0,SAD => Null, SAG => Null, Symbole => -1, Code => Code_Vide);
    end Initialiser_Fin_Fichier;

	procedure Detruire (Arbre : in out T_Arbre) is
	begin
		if Arbre /= Null then
            Detruire (Arbre.all.SAD);
            Detruire (Arbre.all.SAG);
            Free(Arbre);
		end if;
	end Detruire;

    function Est_Vide (Arbre : T_Arbre) return Boolean is
    begin
        return Arbre = Null;
    end;

    function Est_Feuille (Arbre : T_Arbre) return Boolean is
    begin
        if Arbre = Null then
            return False;
        else
            return Arbre.SAD = Null; --l'arbre étant complet, il suffit de ne vérifier qu'une branche
        end if;
    end;

    function Fusionner(SousArbreGauche: in T_Arbre; SousArbreDroit: in T_Arbre) return T_Arbre is
        Nouvelle_Frequence: Integer;
        Nouveau_Code: T_Tableau_Bit;
    begin
        Nouvelle_Frequence := SousArbreGauche.Frequence + SousArbreDroit.Frequence;
        return new T_Noeud'(Frequence => Nouvelle_Frequence, SAG => SousArbreGauche, SAD => SousArbreDroit, Code => Nouveau_Code, Symbole => -1);
    end Fusionner;

    function La_Frequence (Arbre : in T_Arbre) return Integer is
    begin
        return Arbre.all.Frequence;
    end La_Frequence;

    function Le_Code (Arbre : in T_Arbre) return T_Tableau_bit is
    begin
        return Arbre.all.Code;
    end Le_Code;

    function Le_Symbole (Arbre : in T_Arbre ) return Integer is
    begin
        return Arbre.all.Symbole;
    end Le_Symbole;

    function Arbre_Gauche (Arbre : in T_Arbre ) return T_Arbre is
    begin
        return Arbre.all.SAG;
    end Arbre_Gauche;

    function Arbre_Droit (Arbre : in T_Arbre ) return T_Arbre is
    begin
        return Arbre.all.SAD;
    end Arbre_Droit;

    procedure Augmenter_frequence (Arbre: in out T_Arbre) is
    begin
        Arbre.all.Frequence := Arbre.all.Frequence + 1;
    end Augmenter_frequence;

    procedure Mettre_A_Jour_Code(Arbre: in out T_Arbre) is

        procedure MAJ_Code_Rec(Arbre: in out T_Arbre; Code: in out T_Tableau_bit) is
            Code_Droite : T_Tableau_bit;
        begin
            if Est_Feuille(Arbre) then
                if Code.Taille = 0 then
                    Code.Elements(Code.Taille) := 1;
                    Code.Taille := 1;
                end if;
                Arbre.all.Code := Code;
            else
                Code_Droite := Deep_Copy(Code);
                Code.Elements(Code.Taille) := 0;
                Code.Taille := Code.Taille + 1;
                MAJ_Code_Rec(Arbre.all.SAG, code);
                Code_Droite.Elements(Code_Droite.Taille) := 1;
                Code_Droite.Taille := Code_Droite.Taille + 1;
                MAJ_Code_Rec(Arbre.all.SAD, Code_Droite);
            end if;

        end MAJ_Code_Rec;

        Code : T_Tableau_bit;
    begin
        Code.Taille := 0;
        if not(Est_Vide(Arbre)) then
            MAJ_Code_Rec(Arbre,Code);
        end if;
    end Mettre_A_Jour_Code;

    procedure Afficher(Arbre: in T_Arbre) is
        procedure Afficher_Recc (Arbre: in T_Arbre; Parcours : in out T_Tableau_bit) is

            -- Procedure pour bien indenter le noeud dans l'affichage de l'arbre.
            procedure Espace(Parcours : in T_Tableau_bit) is
            begin
                for i in 0..Parcours.Taille-1 loop
                    if Parcours.Elements(i) = 0 then
                        Put(" |     ");
                    else
                        Put("      ");
                    end if;
                end loop;
            end Espace;

            Parcours_Droite : T_Tableau_bit;
        begin
            if not(Est_Vide(Arbre)) then
                -- On écrit la racine du noeud courant.
                Put('(');
                Put(Arbre.all.frequence,1);
                Put(')');

                if Est_Feuille(Arbre) then
                    Afficher_Carac(Arbre.all.Symbole);
                    Put_Line("");
                else
                    Put_Line("");
                    Parcours_Droite := Deep_Copy(Parcours);

                    -- On représente l'arbre gauche
                    Espace(Parcours);
                    Put(" \--0--");
                    Parcours.Elements(Parcours.Taille) := 0;
                    Parcours.Taille := Parcours.Taille + 1;
                    Afficher_Recc(Arbre.all.SAG,Parcours);

                    -- On représente l'arbre droite
                    Espace(Parcours_Droite);
                    Put(" \--1--");
                    Parcours_Droite.Elements(Parcours_Droite.Taille) := 1;
                    Parcours_Droite.Taille := Parcours_Droite.Taille + 1;
                    Afficher_Recc(Arbre.all.SAD,Parcours_Droite);
                end if;
            end if;
        end Afficher_Recc;

        Parcours : T_Tableau_bit;
    begin
        Parcours.Taille := 0;
        Afficher_Recc(Arbre,Parcours);
    end Afficher;

    function Creer_Tableau_Symbole(Arbre: in out T_Arbre) return T_Tableau_Symbole is
        procedure Ajouter_Symboles(Arbre: in T_Arbre; tableau : in out T_Tableau_Symbole) is
        begin
            if Est_Feuille(Arbre) then
                Tableau.Elements(Tableau.Taille).Symbole := Arbre.all.Symbole;
                Tableau.Elements(Tableau.Taille).Code := Arbre.all.Code;
                Tableau.Taille := Tableau.Taille + 1;
            else
                Ajouter_Symboles(Arbre.SAG,tableau);
                Ajouter_Symboles(Arbre.SAD,tableau);
            end if;
        end Ajouter_Symboles;

        Tableau : T_Tableau_Symbole;
    begin
        Mettre_A_Jour_Code(Arbre);
        Tableau.Taille := 0;
        if not(Est_Vide(Arbre)) then
            Ajouter_Symboles(Arbre,Tableau);
        end if;
        return tableau;
    end Creer_Tableau_Symbole;

end Arbre_Huffman;
