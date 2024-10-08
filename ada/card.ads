with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package Card is

    -- Represents a playing card.
    -- This struct defines a card object with a rank and suit.
    type Card is record

        rank : Integer := -1;
        suit : Integer := -1;

    end record;

    function Init_Card (r : Integer; s : Integer) return Card;
    function Card_To_String (c : Card) return String;
    function Compare_Card (c1 : Card; c2 : Card) return Integer;

end Card;