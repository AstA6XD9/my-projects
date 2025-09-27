with Ada.Text_IO;            use Ada.Text_IO;
with SDA_Exceptions;         use SDA_Exceptions;
with Ada.Unchecked_Deallocation;
package body TH is


	procedure Free is
		new Ada.Unchecked_Deallocation (Object =>T_Cellule, Name => T_LCA);

    procedure Initialiser (Sda : out T_TH) is
    begin
        for i in Sda.Tableau'Range loop
            Sda.Tableau(i) := null; -- Initialiser chaque cellule à null
        end loop;
        Sda.Nombre_Elem := 0; -- Réinitialiser le nombre d'éléments
    end Initialiser;

    procedure Detruire (Sda : in out T_TH) is
        Curseur, Suivant : T_LCA;
    begin
        for i in Sda.Tableau'Range loop
            Curseur := Sda.Tableau(i);
            while Curseur /= null loop
                Suivant := Curseur.all.Suivant;
                Free(Curseur); -- Libérer chaque nœud
                Curseur := Suivant;
            end loop;
            Sda.Tableau(i) := null; -- Réinitialiser la cellule
        end loop;
        Sda.Nombre_Elem := 0;
    end Detruire;

    function Est_Vide (Sda : in T_TH) return Boolean is
    begin
        return Sda.Nombre_Elem = 0;
    end Est_Vide;

    function Taille (Sda : in T_TH) return Integer is
    begin
        return Integer(Sda.Nombre_Elem);
    end Taille;

    procedure Enregistrer (Sda : in out T_TH; Cle : in T_Cle; Valeur : in T_Valeur) is
        Index : constant Natural := Fonction_Hachage(Cle);
        Curseur : T_LCA := Sda.Tableau(Index);
    begin

        while Curseur /= null loop
            if Curseur.all.Cle = Cle then
                Curseur.all.Valeur := Valeur;
                return;
            end if;
            Curseur := Curseur.Suivant;
        end loop;

        Sda.Tableau(Index) := new T_Cellule'(Cle => Cle, Valeur => Valeur, Suivant => Sda.Tableau(Index));
        Sda.Nombre_Elem := Sda.Nombre_Elem + 1;
    end Enregistrer;

    procedure Supprimer (Sda : in out T_TH; Cle : in T_Cle) is
        Index : constant Natural := Fonction_Hachage(Cle);
        Curseur, A_Detruire : T_LCA;
    begin
        -- Cas où la clé est en tête de liste
        if Sda.Tableau(Index) /= null and Sda.Tableau(Index).Cle = Cle then
            A_Detruire := Sda.Tableau(Index);
            Sda.Tableau(Index) := Sda.Tableau(Index).Suivant;
            Free(A_Detruire);
            Sda.Nombre_Elem := Sda.Nombre_Elem - 1;
            return;
        end if;

        -- Parcourir la liste pour trouver et supprimer la clé
        Curseur := Sda.Tableau(Index);
        while Curseur /= null and then Curseur.Suivant /= null loop
            if Curseur.Suivant.Cle = Cle then
                A_Detruire := Curseur.Suivant;
                Curseur.Suivant := Curseur.Suivant.Suivant;
                Free(A_Detruire);
                Sda.Nombre_Elem := Sda.Nombre_Elem - 1;
                return;
            end if;
            Curseur := Curseur.Suivant;
        end loop;

        -- Si la clé n'est pas trouvée, lever une exception
        raise Cle_Absente_Exception;
    end Supprimer;

    function Cle_Presente (Sda : in T_TH; Cle : in T_Cle) return Boolean is
        Index : constant Natural := Fonction_Hachage(Cle);
        Curseur : T_LCA := Sda.Tableau(Index);
    begin
        -- Parcourir la liste pour chercher la clé
        while Curseur /= null loop
            if Curseur.Cle = Cle then
                return True; -- Clé trouvée
            end if;
            Curseur := Curseur.Suivant;
        end loop;
        return False; -- Clé absente
    end Cle_Presente;

    function La_Valeur (Sda : in T_TH; Cle : in T_Cle) return T_Valeur is
        Index :constant Natural := Fonction_Hachage(Cle);
        Curseur : T_LCA := Sda.Tableau(Index);
    begin
        -- Parcourir la liste pour chercher la clé
        while Curseur /= null loop
            if Curseur.Cle = Cle then
                return Curseur.Valeur; -- Retourner la valeur associée
            end if;
            Curseur := Curseur.Suivant;
        end loop;

        -- Si la clé n'est pas trouvée, lever une exception
        raise Cle_Absente_Exception;
    end La_Valeur;

    procedure Pour_Chaque (Sda : in T_TH) is
        Curseur : T_LCA;
    begin
        for i in Sda.Tableau'Range loop
            Curseur := Sda.Tableau(i);
            while Curseur /= null loop
                begin
                    Traiter(Curseur.all.Cle, Curseur.all.Valeur);
                exception
                    when others => null;
                end;
                Curseur := Curseur.Suivant;
            end loop;
        end loop;
    end Pour_Chaque;

    procedure Afficher_Debug (Sda : in T_TH) is
        Curseur : T_LCA;
    begin
        for i in Sda.Tableau'Range loop
            Put(Integer'Image(i-1) & " ");
            Curseur := Sda.Tableau(i);
            if Curseur = null then
                Put("--E");
            else
                while Curseur /= null loop
                    Put("-->[""");
                    Afficher_Cle(Curseur.Cle);
                    Put(""" : ");
                    Afficher_Donnee(Curseur.Valeur);
                    Put(" ]");
                    Curseur := Curseur.Suivant;
                end loop;
                Put("--E");
            end if;
            New_Line;
        end loop;
    end Afficher_Debug;

end TH;
