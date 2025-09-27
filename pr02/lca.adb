with Ada.Text_IO;            use Ada.Text_IO;
with SDA_Exceptions;         use SDA_Exceptions;
with Ada.Unchecked_Deallocation;

package body LCA is

	procedure Free is
		new Ada.Unchecked_Deallocation (Object =>T_Cellule, Name => T_LCA);


	procedure Initialiser(Sda: out T_LCA) is
	begin
		SDA:=null;
	end Initialiser;


	procedure Detruire (Sda : in out T_LCA) is
	begin
        if Sda=null then
            null;

        else
            Detruire(Sda.all.suivant);
            Free(Sda);
        end if ;

	end Detruire;


procedure Afficher_Debug (Sda : in T_LCA) is
    Curseur : T_LCA := Sda;
begin
    if Curseur = null then
        Put("--E");
    else
        while Curseur /= null loop
            Put("-->[""");
            Afficher_Cle(Curseur.all.Cle);
            Put(""" : ");
            Afficher_Donnee(Curseur.all.Valeur);
            Put(" ]");
            Curseur := Curseur.all.Suivant;
        end loop;
        Put("--E");
    end if;

end Afficher_Debug;



	function Est_Vide (Sda : T_LCA) return Boolean is
    begin
        return Sda=null;
	end;


    function Taille (Sda : in T_LCA) return Integer is
    begin
        if Est_Vide(Sda) then
            return 0;
        else
            return 1+Taille(Sda.all.Suivant);
        end if;
        end ;


 procedure Enregistrer (Sda : in out T_LCA; Cle : in T_Cle; Valeur : in T_Valeur) is
    Curseur : T_LCA;
begin

    if Sda = null then
        Sda := new T_Cellule'(Cle, Valeur, null);
    else
        Curseur := Sda;
        while Curseur.all.Suivant /= null and then Curseur.all.Cle /= Cle loop
            Curseur := Curseur.all.Suivant;
        end loop;
        if Curseur.all.Cle = Cle then
            Curseur.all.Valeur := Valeur;
        else

            Curseur.all.Suivant := new T_Cellule'(Cle, Valeur, null);
        end if;
    end if;
end Enregistrer;



    function Cle_Presente (Sda : in T_LCA ; Cle : in T_Cle) return Boolean is
        Curseur:T_LCA;
    begin
        Curseur:=Sda;
        while Curseur /= null and then Curseur.all.Cle/=Cle loop
            Curseur:=Curseur.suivant ;
        end loop;
        return Curseur/=null ;
    end Cle_Presente;


function La_Valeur (Sda : in T_LCA; Cle : in T_Cle) return T_Valeur is
    Curseur : T_LCA := Sda;
begin while Curseur /= null and then Curseur.all.Cle /= Cle loop
        Curseur := Curseur.all.Suivant;
    end loop;
    if Curseur = null then
        raise Cle_Absente_Exception;
    else
        return Curseur.all.Valeur;
    end if;
end La_Valeur;



procedure Supprimer (Sda : in out T_LCA; Cle : in T_Cle) is
    Curseur : T_LCA;
    A_Detruire : T_LCA;
    Trouve : Boolean := False;
begin
    -- Suppression en début de liste
    while Sda /= null and then Sda.all.Cle = Cle loop
        A_Detruire := Sda;
        Sda := Sda.all.Suivant;
        Free(A_Detruire);
        Trouve := True;
    end loop;

    -- Suppression dans le reste de la liste
    if Sda /= null then
        Curseur := Sda;
        while Curseur.all.Suivant /= null loop
            if Curseur.all.Suivant.all.Cle = Cle then
                A_Detruire := Curseur.all.Suivant;
                Curseur.all.Suivant := Curseur.all.Suivant.all.Suivant;
                Free(A_Detruire);
                Trouve := True;
            else
                Curseur := Curseur.all.Suivant;
            end if;
        end loop;
    end if;

    -- Lever une exception si la clé n'a pas été trouvée
    if not Trouve then
        raise Cle_Absente_Exception;
    end if;
end Supprimer;





    procedure Pour_Chaque (Sda : in T_LCA) is
        Curseur : T_LCA ;
    begin
        Curseur := Sda ;
        while not Est_Vide(Curseur) loop
            begin
                Traiter(Curseur.all.Cle,Curseur.all.Valeur);
            exception
                when others => null;
            end ;
            Curseur := Curseur.all.Suivant;
        end loop;
    end Pour_Chaque;


end LCA;
