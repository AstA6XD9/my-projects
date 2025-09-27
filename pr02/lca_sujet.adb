with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with LCA;
procedure lca_sujet is
    package LCAS is new LCA(T_CLE => Unbounded_String, T_Valeur => Integer ) ;
    use LCAS;
    procedure Afficher_Donnee_1 (nbr : in Integer) is
    begin
        Put(nbr,1);
    end Afficher_Donnee_1;
    procedure Afficher_Cle_1 (str : in Unbounded_String) is
    begin
        Put(To_String(str));
    end Afficher_Cle_1;
    procedure Afficher is new Afficher_Debug(Afficher_Cle => Afficher_Cle_1 , Afficher_Donnee => Afficher_Donnee_1);

    lca : T_LCA;

begin
    Initialiser(lca);
    Enregistrer(lca , To_Unbounded_String(" un ") ,1);
    Enregistrer(lca , To_Unbounded_String(" deux "),2) ;
    Afficher(lca);
end lca_sujet;
