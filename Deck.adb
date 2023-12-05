with Ada.Text_IO; use Ada.Text_IO;
with Ads.Strings.Unbounded; use Ada.Strings.Unbounded;
with Card; use Card;

type Card_Array is array (Positive range <>) of Card.Card;
type Integer_Array is array(Positive range <>) of Integer;

-- Define a Card type

   type Card is
      record
         Suit : Integer;
         Rank : Integer;
      end record;

-- Representing a collection of a set of standard 52 playing cards, with the possibility of
-- 2 Jokers if set. This class allows you to build a deck either randomly or from a file and
-- provides methods to draw cards from the deck9.

   -- Define a Deck type
   type Deck is
      deck
        cards: Card_Array(1..0);
        duplicate: Card;
        deckType: Integer;
      end deck;


-- Initializes a new Deck with an empty list of cards and a default:
-- deck type of -1
-- duplicate of Nil

 Create_Deck(cards : Card_Array(1..0); duplicate : Card; deckType : Integer) return Deck is
      New_Deck : Deck;
   begin
      New_Deck.cards := cards;
      New_Deck.duplicate := duplicate;
      New_Deck.deckType := -1;
      return New_Deck;
   end Create_Deck;


// Returns a string representation of a singular card.
// return: string representing the card object.

function To_String(card1 : Card) return String is
      Result : String (1 .. 5);
   begin
      case card.Rank is
         when 14    => Result := "A";
         when 11   => Result := "J";
         when 12   => Result := "Q";
         when 13   => Result := "K";
         when others => Result := Integer'Image(card1.Rank);
      end case;

      case Card.Suit is
         when 0   => Result := Result & "H";
         when 1 => Result := Result & "D";
         when 2    => Result := Result & "C";
         when 3   => Result := Result & "S";
      end case;

      return Result;
   end To_String;


// Returns a string representation of the deck.
// return: string representing the deck object.

   function Deck_To_String (deck1 : Deck) return String is
      List : String := "";

      procedure Append_To_List (card1 : Card) is
      begin
         if card1.Rank /= 10 then
            List := List & " ";
         end if;

         List := List & To_String(card1);

         if deck1.Deck = 0 then
            if deck1.Cards'Last /= deck1.Cards'Last then
               List := List & ",";
            else
               List := List & new_line;
            end if;
         elsif deck1.Deck = 1 then
            if deck1.Cards'Last /= deck1.Cards'Last and then (deck1.Cards'Last mod 5) /= 0 then
               List := List & ",";
            else
               List := List & new_line;
            end if;
         end if;
      end Append_To_List;

   begin
      for I in deck1.Cards'Range loop
         Append_To_List(deck1.cards1(I));
      end loop;

      return List;
   end Deck_To_String;


// Builds a random deck based on a standard deck of 52 playing cards without jokers.
// Shuffles the deck to randomize the card order.
// param: j_Flag - Boolean representing if jokers should be added or not.


procedure Build_Rand_Deck (D : in out Deck; J_Flag : Boolean) is
   type Rank_Type is range 2 .. 14;
   type Suit_Type is range 0 .. 3;

   type Card_Type is record
      Rank : Rank_Type;
      Suit : Suit_Type;
   end record;

   procedure Initialize_Card (Rank : Rank_Type; Suit : Suit_Type; C : out Card_Type) is
   begin
      C.Rank := Rank;
      C.Suit := Suit;
   end Initialize_Card;

begin
   D.DeckType := 0;

   for Suit in Suit_Type loop
      for Rank in Rank_Type loop
         declare
            New_Card : Card_Type;
         begin
            Initialize_Card(Rank, Suit, New_Card);
            D.Cards(D.Cards'Last + 1) := New_Card;
         end;
      end loop;
   end loop;

   -- If J_Flag is toggled, add 2 Joker cards.

   if J_Flag then
      declare
         Joker1 : Card_Type;
         Joker2 : Card_Type;
      begin
         Initialize_Card(15, 4, Joker1);
         Initialize_Card(15, 5, Joker2);
         D.Cards(D.Cards'Last + 1) := Joker1;
         D.Cards(D.Cards'Last + 1) := Joker2;
      end;
   end if;
end Build_Rand_Deck;



// Builds a deck based on an input file.
// param: file - String representing the file to build the deck from.

procedure Build_File_Deck (D : in out Deck; File : String) is

   function To_Lower (S : String) return String is
      Result : String := S;
   begin
      for I in Result'Range loop
         Result(I) := To_Lower(Result(I));
      end loop;
      return Result;
   end To_Lower;

   Content : String := "";
   Lines   : Ada.Strings.Unbounded.Unbounded_String;
   Line    : String;
   S      : String;
   Rank   : Rank_Type;
   Suit   : Suit_Type;

begin
   D.Deck := 1;

   -- Read in the content of the file

   begin
      declare
         File_Handle : File_Type;
      begin
         Open (File_Handle, In_File, File);
         while not End_Of_File (File_Handle) loop
            Read_Line (File_Handle, Line);
            Append (Lines, Line);
         end loop;
         Close (File_Handle);
      end;
   exception
      when others =>
         Put_Line ("Error reading file: " & File);
         return;
   end;

   -- Process each line in the file

   for I in 1 .. Ada.Strings.Unbounded.Length (Lines) loop
      Line := To_Lower (Ada.Strings.Unbounded.To_String (Ada.Strings.Unbounded.Element (Lines, I)));
      S := "";
      for J in 1 .. Line'Length loop
         if Line (J) /= ' ' then
            S := S & Line (J);
         end if;
      end loop;

      -- Parse rank
      case S (2) is
         when '1' => Rank := 10;
         when 'j' => Rank := 11;
         when 'q' => Rank := 12;
         when 'k' => Rank := 13;
         when 'a' => Rank := 14;
         when 'x' => Rank := 15;
         when others =>
            declare
               Val : Integer;
            begin
               Ada.Text_IO.Get (S (2), Val);
               Rank := Rank_Type (Val);
            exception
               when others =>
                  Put_Line ("Error parsing rank: " & S);
                  return;
            end;
      end case;

      -- Parse suit
      case S (4) is
         when 'd' => Suit := 0;
         when 'c' => Suit := 1;
         when 'h' => Suit := 2;
         when 's' => Suit := 3;
         when 'r' => Suit := 4;
         when 'b' => Suit := 5;
         when others =>
            Put_Line ("Error parsing suit: " & S);
            return;
      end case;

      -- Check for duplicates
      for Card in D.Cards loop
         if Card.Rank = Rank and Card.Suit = Suit then
            D.Duplicate := Card;
         end if;
      end loop;

      -- Append the card to the deck
      Initialize_Card (Rank, Suit, D.Cards (D.Cards'Last + 1));
   end loop;
end Build_File_Deck;




// Draws a card from the deck, removing the first card.
// return: Card representing the removed card.
procedure Draw_Card (D : in out Deck; Ret : out Card_Type) is
begin
   Ret := D.Cards (1);
   D.Cards := D.Cards (2 .. D.Cards'Last);
end Draw_Card;
