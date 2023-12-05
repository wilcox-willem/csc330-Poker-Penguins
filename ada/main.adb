with Ada.Text_IO; use Ada.Text_IO;
with Card; use Card;
with Hand; use Hand;

-- Main program
procedure Main is

    c1 : Card.Card;
    c2 : Card.Card;
    Difference : Integer;

    h1 : Hand.Hand;
    h2 : Hand.Hand;

begin

    -- Card Tests
    c1 := Init_Card(10, 1); --
    c2 := Init_Card(12, 3); -- Queen of spades
    
    Put_Line("Card 1: " & Card_To_String(c1));
    Put_Line("Card 2: " & Card_To_String(c2));
    
    Difference := Compare_Card(c1, c2);
    
    Put_Line("Difference in ranks: " & Integer'Image(Difference));

    h1 := Init_Hand;
    h2 := Init_Hand;

    Add_Card(h1, c1);


    Put_Line("Hand 1: " & Hand_To_String(h1));
    Put_Line("Hand 2: " & Hand_To_String(h2));

end Main;