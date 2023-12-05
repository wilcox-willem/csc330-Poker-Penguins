package body Card is

    -- Initializes a new card with the specified rank and suit.
    function Init_Card (r : Integer; s : Integer) return Card is
        newCard : Card;
    begin

        newCard.rank := r;
        newCard.suit := s;

        return newCard;

    end Init_Card;


    -- Returns a string representation of the card.
    function Card_To_String (c : Card) return String is
        suitLabel : Unbounded_String;
        face      : Unbounded_String := To_Unbounded_String("");
        rankStr   : Unbounded_String := To_Unbounded_String(Integer'Image(c.rank));
    begin

        if    c.suit = 0 then suitLabel := To_Unbounded_String("D");
        elsif c.suit = 1 then suitLabel := To_Unbounded_String("C");
        elsif c.suit = 2 then suitLabel := To_Unbounded_String("H");
        elsif c.suit = 3 then suitLabel := To_Unbounded_String("S");
        elsif c.suit = 4 then suitLabel := To_Unbounded_String("R");
        elsif c.suit = 5 then suitLabel := To_Unbounded_String("B");
        end if;

        if    c.rank = 10 then face := To_Unbounded_String("10"); 
        elsif c.rank = 11 then face := To_Unbounded_String("J");
        elsif c.rank = 12 then face := To_Unbounded_String("Q");
        elsif c.rank = 13 then face := To_Unbounded_String("K");
        elsif c.rank = 14 then face := To_Unbounded_String("A");
        elsif c.rank = 15 then face := To_Unbounded_String("X");
        end if;

        if face = To_Unbounded_String("") then
            Append(rankStr, suitLabel);
            return To_String(rankStr);

        else
            Append(face, suitLabel);
            return To_String(face);

        end if;

    end Card_To_String;


    -- Compares this card with another card based on their ranks.
    function Compare_Card (c1 : Card; c2 : Card) return Integer is
    begin

        return c1.rank - c2.rank;

    end Compare_Card;

end Card;