with Ada.Text_IO; use Ada.Text_IO;
package body Hand is

    function Init_Hand return Hand is
        newHand : Hand;
    begin

        newHand.handType := 0;
        return newHand;
    
    end Init_Hand;



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



    function Compare_Hand(hand1, hand2 : Hand) return Integer is

        typeComparison : Integer;

    begin

        assessHand(hand1);
        assessHand(hand2);

        typeComparison := hand1.handType - hand2.handType;

        return CompareHelper(hand1, hand2, typeComparison, 0);
    
    end Compare_Hand;



    function CompareHelper(hand1, hand2 : Hand; diff : Integer; pass : Integer) return Integer is
        
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
        rankDiff := thisBreaker.suit - otherBreaker.suit;

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



    --  function isRSF(h : Hand) return Boolean is

    --  begin

    --  return null;
    
    --  end isRSF;



    --  function isSF(h : Hand) return Boolean is

    --  begin

    --  return null;
    
    --  end isSF;



    --  function isFOAK(h : Hand) return Boolean is

    --  begin

    --  return null;
    
    --  end isFOAK;



    --  function isFH(h : Hand) return Boolean is

    --  begin

    --  return null;
    
    --  end isFH;



    --  function isF(h : Hand) return Boolean is

    --  begin

    --  return null;
    
    --  end isF;



    --  function isS(h : Hand) return Boolean is

    --  begin

    --  return null;
    
    --  end isS;



    --  function isTOAK(h : Hand) return Boolean is

    --  begin

    --  return null;
    
    --  end isTOAK;



    --  function isTP(h : Hand) return Boolean is

    --  begin

    --  return null;
    
    --  end isTP;



    --  function isP(h : Hand) return Boolean is

    --  begin

    --  return null;
    
    --  end isP;



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



    --  --  function generateAvailableCards(h : Hand; color : Integer) return Card_Array is

    --  --  begin

    --  --  return null;
    
    --  --  end generateAvailableCards;



    --  function contains(arr : Card_Array; rank : Integer; suit : Integer) return Boolean is

    --  begin

    --  return null;

    --  end contains;



    --  procedure assessHand(h : Hand) is

    --  begin

    --  end assessHand; 



    procedure Add_Card(h : in out Hand; c : Card.Card) is
    begin

        for i in h.cards'Range loop
            if (h.cards(i).rank = -1) then
                h.cards(i) := Init_Card(c.rank, c.suit);
                exit;

            end if;
        end loop;

    end Add_Card;



    procedure Sort_Hand(H: in out Hand) is
       Temp: Card.Card;
    begin
       H.Sorted := (others => (Rank => 0, Suit => 0));
    
       for I in H.Cards'Range loop
          H.Sorted(I) := Init_Card(H.Cards(I).Rank, H.Cards(I).Suit);
       end loop;
    
       for J in 1 .. 4 loop
          for I in 1 .. 4 loop
             if Compare_Card(H.Sorted(I), H.Sorted(I + 1)) > 0 then 
                Temp := Init_Card(H.Sorted(I + 1).Rank, H.Sorted(I + 1).Suit);
                H.Sorted(I + 1) := H.Sorted(I);
                H.Sorted(I) := Temp;
             end if;
          end loop;
       end loop;
    end Sort_Hand;


end Hand;

