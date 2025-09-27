with Ada.Text_IO;            use Ada.Text_IO;
with Arbre_Huffman;          use Arbre_Huffman;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;

package body ToolBox is

    procedure Afficher_Tab_Bit(Tab : in T_Tableau_bit) is
    begin
        Put ("[ ");
        if Tab.Taille /= 0 then
            Put(Integer(Tab.Elements(0)), 1);
        end if;
        for I in 1..Tab.Taille-1 loop
            Put(" , ");
            Put(Integer(Tab.Elements(I)), 1);
        end loop;
        Put(" ]");
    end Afficher_Tab_Bit;

    procedure Afficher_Tab_int(Tab : in T_Tableau_entier) is
    begin
        Put ("[ ");
        if Tab.Taille /= 0 then
            Put(Tab.Elements(0), 1);
        end if;
        for I in 1..Tab.Taille-1 loop
            Put(" , ");
            Put(Tab.Elements(I), 1);
        end loop;
        Put(" ]");
    end Afficher_Tab_int;

    procedure Afficher_Tab_symboles(Tab : in T_Tableau_entier) is
    begin
        Put ("[ ");
        if Tab.Taille /= 0 then
            Afficher_Carac(Tab.Elements(0));
        end if;

        for i in 1..Tab.Taille-1 loop
            Put(" , ");
            Afficher_Carac(Tab.Elements(i));
        end loop;
        Put(" ]");

    end Afficher_Tab_symboles;

    procedure Afficher_Carac(Code: Integer) is
    begin
        Put(" '");
        if Code = -1 then
            Put("\$");
        elsif Code = 10 then
            Put("\" & "n");
        else
            Put(Character'Val(Code));
        end if;
        Put("'");
    end Afficher_Carac;

    function To_Binaire(symbole : in T_Octet ) return T_Tableau_bit is
        Tableau : T_Tableau_bit ;
        Octet : T_Octet;
    begin
        Octet := symbole;
        Tableau.Taille := 8;
        for I in 0..7 loop
            Tableau.Elements(I) := T_bit(Octet / 128); -- Extraction du bit de poids fort
            Octet := Octet * 2; -- Décalage vers la gauche
        end loop;
        return Tableau;
    end To_Binaire;

end ToolBox;
