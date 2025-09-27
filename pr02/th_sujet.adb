with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Integer_Text_IO;       use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with TH;

procedure th_sujet is


    function Generic_Hash(Cle : Unbounded_String) return Natural is
        longueur : Integer ;
    begin
        longueur := length(Cle);
        return longueur mod 11 +1 ;
    end Generic_Hash;


    package THS is new TH(
        T_Cle => Unbounded_String,
        T_Valeur => Integer,
        Taille_tableau => 11,
        Fonction_Hachage => Generic_Hash
    );
    use THS;

    procedure Afficher_Donnee_1 (nbr : in Integer) is
    begin
        Put(nbr, 1);
    end Afficher_Donnee_1;

    procedure Afficher_Cle_1 (str : in Unbounded_String) is
    begin
        Put(To_String(str));
    end Afficher_Cle_1;

    procedure Afficher is new Afficher_Debug(Afficher_Cle => Afficher_Cle_1, Afficher_Donnee => Afficher_Donnee_1);

    th : T_TH;

    type Cle_array is array(1..7) of Unbounded_String;
    type Valeur_array is array(1..7) of Integer;

    Cle : constant Cle_array := (To_Unbounded_String("un"), To_Unbounded_String("deux"), To_Unbounded_String("trois"), To_Unbounded_String("quatre"), To_Unbounded_String("cinq"), To_Unbounded_String("quatre-vingt-dix-neuf"), To_Unbounded_String("vingt-et-un"));
    Valeur : constant Valeur_array := (1, 2, 3, 4, 5, 99, 21);

begin
    Initialiser(th);

    for i in Cle'range loop
        Enregistrer(th, Cle(i), Valeur(i));
    end loop;

    Afficher(th);

    Detruire(th);
end th_sujet;
