package body Deck is

--  Initializes a new Deck
    function Init_Deck return Deck is
        newDeck : Deck;
    begin
        newDeck.deckType := -1;
        return newDeck;
    end Init_Deck;


--  Returns a string representation of the deck.
    function Deck_To_String (d : Deck) return String is
        list : Unbounded_String;
    begin
        for i in d.cards'Range loop
            if not (d.cards(i).rank = -1) then

                if (d.cards(i).rank /= 10) then
                    Append(list, To_Unbounded_String(" "));
                end if;

                Append(list, To_Unbounded_String(Card_To_String(d.cards(i))));

                if (i = d.cards'Length) then
                    exit;
                elsif ((i = 1 or else i mod 13 /= 0) and then d.deckType = 0) then
                    Append(list, To_Unbounded_String(","));
                elsif ((i = 1 or else i mod 5 /= 0) and then d.deckType = 1) then
                    Append(list, To_Unbounded_String(","));
                else
                    Append(list, Character'Val(10));
                end if;

            end if;

        end loop;

        return To_String(list);
    end Deck_To_String;





-- Builds a random deck based on a standard deck of 52 playing cards without jokers.
-- Shuffles the deck to randomize the card order.
procedure Build_Rand_Deck (d : in out Deck; j_Flag : Boolean) is
    index : Integer := 1;
    temp : Card.Card;
begin
   d.deckType := 0;

   for suit in 0..3 loop

      for rank in 2..14 loop

        d.cards(index) := Init_Card(rank, suit);
        index := index + 1;

      end loop;

   end loop;

   -- If J_Flag is toggled, add 2 Joker cards.

   if j_Flag then
        d.cards(index) := Init_Card(15, 4);
        d.cards(index + 1) := Init_Card(15, 5);
   end if;

   -- shuffles deck
    -- need to implement
   
end Build_Rand_Deck;






-- Builds a deck based on an input file.
--  procedure Build_File_Deck (d : in out Deck; file : String) is
--      type UnbStr_Array is array(Positive range <>) of Unbounded_String;
--      Rank, Suit, index : Integer;
--      Content : Unbounded_String;
--      Lines   : UnbStr_Array(1..6);
--      Line    : Unbounded_String;
--      S       : Unbounded_String;
--      Pos : Natural;

--  begin
--     d.deckType := 1;

--     -- Read in the content of the file
--      Ada.Text_IO.Open(File => file, Mode => Ada.Text_IO.In_File, Item => Content);
--      Ada.Strings.Unbounded.Split(Content, Lines);

--     for I in 1 .. Ada.Strings.Unbounded.Length(Lines) loop
--              Ada.Strings.Unbounded.Extract(Line, Lines, I);
--              S := Ada.Strings.Unbounded.To_String(Line);

--              while S /= "" loop
--                  -- Trimming any leading spaces
--                  Pos := Ada.Strings.Index(S, Ada.Strings.To_Unbounded_String(" "));
--                  if Pos /= 0 then
--                      S := Ada.Strings.Right(S, S'Last - Pos);
--                  end if;

--                  -- Extracting Rank and Suit information
--                  Rank := 0;
--                  Suit := 0;

--                  case S(1) is
--                      when '1' => Rank := 10;
--                      when 'J' => Rank := 11;
--                      when 'Q' => Rank := 12;
--                      when 'K' => Rank := 13;
--                      when 'A' => Rank := 14;
--                      when 'X' => Rank := 15;
--                      when others =>
--                          Rank := Integer'Value(S(1..1));
--                  end case;

--                  case S(3) is
--                      when 'D' => Suit := 0;
--                      when 'C' => Suit := 1;
--                      when 'H' => Suit := 2;
--                      when 'S' => Suit := 3;
--                      when 'R' => Suit := 4;
--                      when 'B' => Suit := 5;
--                  end case;

--                  for i in d.cards'Range loop
--                      if (c.cards(i).rank = Rank and c.cards(i).suit = suit) then
--                          d.duplicate := c.cards(i);
--                      end if;
--                  end loop;

--                  -- Append the initialized card to the deck
--                  d.cards(index) := Init_Card(Rank, Suit);
--              end loop;
--          end loop;

--          Ada.Text_IO.Close(file);

--      end Build_File_Deck;




--  -- Draws a card from the deck, removing the first card.
--      function Draw_Card (d : in out Deck) return Card.Card is
--          ret : Card.Card := d.cards(1;)
--      begin
--          d.cards := d.cards (2 .. d.cards'Last);
--      end Draw_Card;

end Deck;