with Ada.Text_IO; use Ada.Text_IO;
with Card; use Card;
with Hand; use Hand;

-- Main program
procedure Main is

    c1, c2, c3, c4, c5 : Card.Card;
    Difference : Integer;

    h1 : Hand.Hand;
    h2 : Hand.Hand;

begin

    -- Card Tests
    c1 := Init_Card(15, 5);
    c2 := Init_Card(14, 3);
    c3 := Init_Card(13, 3);
    c4 := Init_Card(11, 3); 
    c5 := Init_Card(10, 3);
    
    Put_Line("Card 1: " & Card_To_String(c1));
    Put_Line("Card 2: " & Card_To_String(c2));
    Put_Line("Card 3: " & Card_To_String(c3));
    Put_Line("Card 4: " & Card_To_String(c4));
    Put_Line("Card 5: " & Card_To_String(c5));
    
    --Difference := Compare_Card(c1, c2);
    
    --Put_Line("Difference in ranks: " & Integer'Image(Difference));

    h1 := Init_Hand;

    Add_Card(h1, c1);
    Add_Card(h1, c2);
    Add_Card(h1, c3);
    Add_Card(h1, c4);
    Add_Card(h1, c5);

    Put_Line("Hand 1: " & Hand_To_String(h1));

    assessHand(h1);

    Put_Line("Hand 1: " & Hand_To_String(h1));

end Main;