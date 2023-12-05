--  with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
--  with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Card; use Card;

-- Representing a collection of a set of standard 52 playing cards, with the possibility of
-- 2 Jokers if set. This class allows you to build a deck either randomly or from a file and
-- provides methods to draw cards from the deck9.
package Deck is
    type Card_Array is array(Positive range <>) of Card.Card;
    type Integer_Array is array(Positive range <>) of Integer;

    type Deck is record

        cards     : Card_Array(1..54);
        duplicate : Card.Card;
        deckType  : Integer;

    end record;

    function Init_Deck return Deck;
    function Deck_To_String(d : Deck) return String;
    --  function Draw_Card(d : in out Deck) return Card.Card;

    procedure Build_Rand_Deck(d : in out Deck; j_Flag : Boolean);
    --  procedure Build_File_Deck(d : in out Deck; file : String);

end Deck;