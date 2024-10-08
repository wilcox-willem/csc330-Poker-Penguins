with Ada.Text_IO; use Ada.Text_IO;
package body Hand is

--  Initializes a new hand with an empty list of cards, its sorted variant, and a default hand type of 0.
    function Init_Hand return Hand is
        newHand : Hand;
    begin

        newHand.handType := 0;
        return newHand;
    
    end Init_Hand;


--  Returns a string representation of the hand.
    function Hand_To_String(h : Hand) return String is
        list : Unbounded_String;
    begin

        for i in h.cards'Range loop
            if h.cards(i).rank /= 10 then
                Append(list, To_Unbounded_String(" "));
            end if;

            Append(list, To_Unbounded_String(Card_To_String(h.cards(i))));
            

            if i /= 0 and then i mod 5 /= 0 then
                Append(list, To_Unbounded_String(" "));
            end if;
        end loop;

        if h.handType = 0 then
            return To_String(list);

        elsif h.handType = 10 then
            Append(list, To_Unbounded_String(" - Royal Straight Flush"));
            return To_String(list);

        elsif h.handType = 9 then
            Append(list, To_Unbounded_String(" - Straight Flush"));
            return To_String(list);

        elsif h.handType = 8 then
            Append(list, To_Unbounded_String(" - Four of a Kind"));
            return To_String(list);

        elsif h.handType = 7 then
            Append(list, To_Unbounded_String(" - Full House"));
            return To_String(list);

        elsif h.handType = 6 then
            Append(list, To_Unbounded_String(" - Flush"));
            return To_String(list);

        elsif h.handType = 5 then
            Append(list, To_Unbounded_String(" - Straight"));
            return To_String(list);

        elsif h.handType = 4 then
            Append(list, To_Unbounded_String(" - Three of a Kind"));
            return To_String(list);

        elsif h.handType = 3 then
            Append(list, To_Unbounded_String(" - Two Pair"));
            return To_String(list);

        elsif h.handType = 2 then
            Append(list, To_Unbounded_String(" - Pair"));
            return To_String(list);

        else
            Append(list, To_Unbounded_String(" - High Card"));
            return To_String(list);

        end if;
    
    end Hand_To_String;


--  Compares this hand with another hand based on their ranks.
--   Used to sort each hand type and tiebreakers accordingly.
    function Compare_Hand(hand1, hand2 : in out Hand) return Integer is
        typeComparison : Integer;
    begin

        assessHand(hand1);
        assessHand(hand2);

        typeComparison := hand1.handType - hand2.handType;

        return CompareHelper(hand1, hand2, typeComparison, 0);
    
    end Compare_Hand;


--  Recursive helper method for the compare_hand method.
    function CompareHelper(hand1, hand2 : in out Hand; diff : Integer; pass : Integer) return Integer is
        
        thisBreaker : Card.Card;
        otherBreaker : Card.Card;
        rankDiff : Integer;
        suitDiff : Integer;

    begin

        if (diff /= 0) then
            return diff;
        end if;

        thisBreaker := getTieBreakerCard(hand1, pass);
        otherBreaker := getTieBreakerCard(hand2, pass);

        rankDiff := thisBreaker.rank - otherBreaker.rank;
        suitDiff := thisBreaker.suit - otherBreaker.suit;

        if (pass < 2 and (hand1.handType = 2 or hand1.handType = 3)) then
            if (rankDiff = 0) then
                return CompareHelper(hand1, hand2, rankDiff, pass + 1);
            end if;

            return rankDiff;
        end if;

        if (rankDiff = 0) then
            return suitDiff;
        end if;

        return rankDiff;
    
    end CompareHelper;


--  Determines if the hand is a royal straight flush.
    function isRSF(h : Hand) return Boolean is
        rankList : Integer_Array := getRankList(h);
    begin

        return isSF(h) and rankList(1) = 10 and rankList(5) = 14;
    
    --  end isRSF;


--  Determines if the hand is a straight flush.
    function isSF(h : Hand) return Boolean is
    begin

        return isS(h) and isF(h);
    
    --  end isSF;


--  Determines if the hand is a straight flush.
    function isFOAK(h : Hand) return Boolean is
        rankList : Integer_Array := getRankList(h);
    begin

        return (rankList(1) = rankList(4)) or (rankList(2) = rankList(5));
    
    --  end isFOAK;


--  Determines if the hand is a full house.
    function isFH(h : Hand) return Boolean is
        rankList : Integer_Array := getRankList(h);
    begin

        return ((rankList(1) = rankList(2)) and (rankList(3) = rankList(5))) or ((rankList(1) = rankList(3)) and (rankList(4) = rankList(5)));
    
    --  end isFH;


--  Determines if the hand is a flush.
    function isF(h : Hand) return Boolean is
        suit : Integer := h.sorted(1).suit;
    begin

        for i in 1..4 loop
            if (h.sorted(i).suit /= suit) then
                return false;
            end if;
        end loop;

        return h.sorted(5).suit = suit;
    
    --  end isF;


--  Determines if the hand is a straight.
    function isS(h : Hand) return Boolean is
        rankList : Integer_Array := getRankList(h);
        temp : Integer;
    begin

        if (rankList(5) = 14 and rankList(1) = 2) then
            rankList(5) := 1;
            for j in 1 .. 4 loop
                for i in 1 .. 4 loop
                    if rankList(i) - rankList(i + 1) > 0 then
                        temp := rankList(i + 1);
                        rankList(i + 1) := rankList(i);
                        rankList(i) := temp;
                    end if;
                end loop;
            end loop;
        end if;

        for i in 1..4 loop
            if (rankList(i + 1) /= rankList(i) + 1) then
                return false;
            end if;
        end loop;
    
        return true;
    end isS;


--  Determines if the hand is a three of a kind.
    function isTOAK(h : Hand) return Boolean is
        rankList : Integer_Array := getRankList(h);
    begin

        return (rankList(1) = rankList(3)) or
               (rankList(2) = rankList(4)) or
               (rankList(3) = rankList(5));
    
    --  end isTOAK;


--  Determines if the hand is a two pair.
    function isTP(h : Hand) return Boolean is
        rankList : Integer_Array := getRankList(h);
    begin

        return (rankList(1) = rankList(2) and rankList(3) = rankList(4)) or
		       (rankList(1) = rankList(2) and rankList(4) = rankList(5)) or
		       (rankList(2) = rankList(3) and rankList(4) = rankList(5));
    
    --  end isTP;


--  Determines if the hand is a pair.
    function isP(h: Hand) return Boolean is
       RankList : Integer_Array := getRankList(h);
    begin
       for I in 1..4 loop
          if RankList(I) = RankList(I + 1) then
             return True;
          end if;
       end loop;
    
       return False;
    end isP; 


--  Determines the tie breaking card of the hand depending on its handType.
    function getTieBreakerCard(h : Hand; pass : Integer) return Card.Card is

        ret : Card.Card;
        pairList : Card_Array(1..5);
        max : Card.Card;
        min : Card.Card;
        currentCard : Card.Card;
        previousCard : Card.Card;

    begin

        if h.handType = 10 then
            return h.sorted(5);

        elsif h.handType = 9 then
            if h.sorted(5).rank = 14 and h.sorted(1).rank = 2 then
                return h.sorted(4);
            else
                return h.sorted(5);
            end if;

        elsif h.handType = 8 then
            return h.sorted(3);

        elsif h.handType = 7 then
            return h.sorted(3);

        elsif h.handType = 6 then
            return h.sorted(5);

        elsif h.handType = 5 then
            return h.sorted(5);

        elsif h.handType = 4 then
            return h.sorted(3);

        elsif h.handType = 3 then

            for i in 2 .. 5 loop
                currentCard := h.sorted(i);
                previousCard := h.sorted(i - 1);

                if currentCard.rank = previousCard.rank then

                    for k in pairList'Range loop
                        if (pairList(k).rank = -1) then
                            pairList(k) := currentCard;
                            exit;
                        end if;
                    end loop;

                end if;
            end loop;

            max := pairList(1);
            min := pairList(1);

            for j in pairList'Range loop
                if (pairList(j).rank > max.rank) then
                    max := pairList(j);
                end if;
                if (pairList(j).rank < min.rank) then
                    min := pairList(j);
                end if;
            end loop;

            if pass = 0 then
                return max;
            elsif pass = 1 then
                return min;
            else
                return getKicker(h);
            end if;

        elsif h.handType = 2 then

            for k in 2 .. 5 loop
                currentCard := h.sorted(k);
                previousCard := h.sorted(k - 1);

                if currentCard.rank = previousCard.rank then
                    ret := h.sorted(k);
                    exit;
                end if;

            end loop;

            if pass = 0 then
                return ret;
            elsif pass = 1 then
                return getKicker(h);
            end if;
        end if;

        return h.sorted(5);
    
    end getTieBreakerCard;


--  Helper method to get a sorted list of ranks in the hand.
    function getRankList(h : Hand) return Integer_Array is
        rankList : Integer_Array(1..5);
        temp : Integer;
    begin

        for i in h.cards'Range loop
            rankList(i) := h.cards(i).rank;
        end loop;

        for j in 1 .. 4 loop
            for i in 1 .. 4 loop
                if rankList(i) - rankList(i + 1) > 0 then
                    temp := rankList(i + 1);
                    rankList(i + 1) := rankList(i);
                    rankList(i) := temp;
                end if;
            end loop;
        end loop;

        return rankList;
    
    end getRankList;


--  Gets the kicker card for pairs and two pairs.
    function getKicker(h : Hand) return Card.Card is
        nonPairList : Card_Array(1..5);
        max : Card.Card;
    begin

        for i in 2 .. 5 loop
            if h.sorted(i).rank /= h.sorted(i - 1).rank then
                nonPairList(i) := h.sorted(i);
            end if;
        end loop;

        max := nonPairList(1);

        for j in nonPairList'Range loop
            if (nonPairList(j).rank > max.rank) then
                max := nonPairList(j);
            end if;
        end loop;

        return max;
    
    end getKicker;


--  Helper method to see if a certain card of rank r and suit s is inside the array.
    function Contains (arr : Card_Array; rank : Integer; suit : Integer) return Boolean is
    begin
       for I in arr'Range loop
          if arr(I).rank = rank and arr(I).suit = suit then
             return True;
          end if;
       end loop;

       return False;
    end Contains;


--  Generates an array to keep track of every card that can be used as a joker.
    function generateAvailableCards(h : Hand; color : Integer) return Card_Array is
       Avail_Cards : Card_Array(1..26);
       Index : Integer := 1;
       newC : Card.Card;
    begin
       for suit in 0..3 loop
          if ((suit = 0 or suit = 2) and Color = 4) or
             ((suit = 1 or suit = 3) and Color = 5) then
             for rank in 2..14 loop
                if not Contains(h.cards, rank, suit) then
                   Avail_Cards(Index) := Init_Card(rank, suit);
                   Index := Index + 1;
                end if;
             end loop;
          end if;
       end loop;
       return Avail_Cards;
    end generateAvailableCards;

       for s in 0..3 loop
          if ((s = 0 or s = 2) and color = 4) or ((s = 1 or s = 3) and color = 5) then

             for r in 2..14 loop
                if (Contains(h.cards, r, s)) then
                    null;
                end if;

                newC := Init_Card(r, s);
                Avail_Cards(Index) := newC;
                Index := Index + 1;

             end loop;

          end if;

       end loop;

       return Avail_Cards;

    end generateAvailableCards;


--  Analyzes the current collection of cards in the hand and determines its hand type.
--  The sorted instance variable is updated to contain the cards sorted by rank.
    procedure assessHand(h : in out Hand) is
        jokerIndex1 : Integer := -1;
        jokerIndex2 : Integer := -1;

        jokerSuit1 : Integer := 0;
        jokerSuit2 : Integer := 0;

        availCards1 : Card_Array(1..26);
        availCards2 : Card_Array(1..26);

        topScore : Integer := 0;
        currScore : Integer := 0;
        topSorted : Card_Array(1..5);
        
    begin

        for i in h.cards'Range loop
            if (h.cards(i).rank = 15) then
                if (jokerIndex1 = -1) then
                    jokerIndex1 := i;
                    jokerSuit1 := h.cards(i).suit;
                    availCards1 := generateAvailableCards(h, jokerSuit1);
                else
                    jokerIndex2 := i;
                    jokerSuit2 := h.cards(i).suit;
                    availCards2 := generateAvailableCards(h, jokerSuit2);
                    exit;
                end if;
            end if;
        end loop;

        if (jokerIndex1 /= -1) then
            for i in availCards1'Range loop
                for j in availCards2'Range loop
                    h.cards(jokerIndex1) := Init_Card(availCards1(i).rank, availCards1(i).suit);

                    if (jokerIndex2 /= -1) then
                        h.cards(jokerIndex2) := Init_Card(availCards2(j).rank, availCards2(j).suit);
                    end if;

                    sortHand(h);

                    if (isRSF(h)) then currScore := 10;
                    elsif (isSF(h)) then currScore := 9;
                    elsif (isFOAK(h)) then currScore := 8;
                    elsif (isFH(h)) then currScore := 7;
                    elsif (isF(h)) then currScore := 6;
                    elsif (isS(h)) then currScore := 5;
                    elsif (isTOAK(h)) then currScore := 4;
                    elsif (isTP(h)) then currScore := 3;
                    elsif (isP(h)) then currScore := 2;
                    else currScore := 1;
                    end if;

                    h.cards(jokerIndex1) := Init_Card(15, jokerSuit1);

                    if (jokerIndex2 /= -1) then
                        h.cards(jokerIndex2) := Init_Card(15, jokerSuit2);
                    end if;

                    if (currScore > topScore) then
                        topScore := currScore;
                        topSorted := h.sorted;
                    end if;

                    if (jokerIndex2 = -1) then
                        exit;
                    end if;
                end loop;
            end loop;

            h.handType := topScore;
            h.sorted := topSorted;
        else
            sortHand(h);

            if (isRSF(h)) then h.handType := 10;
            elsif (isSF(h)) then h.handType := 9;
            elsif (isFOAK(h)) then h.handType := 8;
            elsif (isFH(h)) then h.handType := 7;
            elsif (isF(h)) then h.handType := 6;
            elsif (isS(h)) then h.handType := 5;
            elsif (isTOAK(h)) then h.handType := 4;
            elsif (isTP(h)) then h.handType := 3;
            elsif (isP(h)) then h.handType := 2;
            else h.handType := 1;
            end if;
        end if;

    end assessHand;


--  Adds a card to the hand's list of cards.
    procedure Add_Card(h : in out Hand; c : Card.Card) is
    begin

        for i in h.cards'Range loop
            if (h.cards(i).rank = -1) then
                h.cards(i) := Init_Card(c.rank, c.suit);
                exit;

            end if;
        end loop;

    end Add_Card;


--  Sets the sorted instance variable to a sorted version of a provided hand.
    procedure sortHand(h: in out Hand) is

       Temp: Card.Card;

    begin
    
       for I in h.cards'Range loop
          h.sorted(I) := Init_Card(h.cards(I).rank, h.cards(I).suit);
       end loop;
    
       for J in 1 .. 4 loop
          for I in 1 .. 4 loop
             if Compare_Card(h.sorted(I), h.sorted(I + 1)) > 0 then 
                Temp := Init_Card(h.sorted(I + 1).rank, h.sorted(I + 1).suit);
                h.sorted(I + 1) := h.sorted(I);
                h.sorted(I) := Temp;
             end if;
          end loop;
       end loop;

    end sortHand;

end Hand;