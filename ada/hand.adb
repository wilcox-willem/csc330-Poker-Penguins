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



    function Compare_Hand(hand1, hand2 : in out Hand) return Integer is

        typeComparison : Integer;

    begin

        assessHand(hand1);
        assessHand(hand2);

        typeComparison := hand1.handType - hand2.handType;

        return CompareHelper(hand1, hand2, typeComparison, 0);
    
    end Compare_Hand;



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



    function isRSF(h : Hand) return Boolean is
        rankList : Integer_Array := getRankList(h);
    begin

        return isSF(h) and rankList(1) = 10 and rankList(5) = 14;
    
    end isRSF;



    function isSF(h : Hand) return Boolean is
    begin

        return isS(h) and isF(h);
    
    end isSF;



    function isFOAK(h : Hand) return Boolean is
        rankList : Integer_Array := getRankList(h);
    begin

        return (rankList(1) = rankList(4)) || (rankList(2) = rankList(5));
    
    end isFOAK;



    function isFH(h : Hand) return Boolean is
        rankList : Integer_Array := getRankList(h);
    begin

        return (rankList(1) = rankList(2)) and (rankList(3) = rankList(5)) or
               (rankList(1) = rankList(3)) and (rankList(4) = rankList(5));
    
    end isFH;



    function isF(h : Hand) return Boolean is
        suit : Integer := h.sorted(1).suit;
    begin

        for i in 1..4 loop
            if (h.sorted(i).suit /= suit) then
                return false;
            end if;
        end loop;

        return h.sorted(5).suit = suit;
    
    end isF;



    function isS(h : Hand) return Boolean is
        rankList : Integer_Array := getRankList(h);
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



    function isTOAK(h : Hand) return Boolean is
        rankList : Integer_Array := getRankList(h);
    begin

        return (rankList[1] = rankList[3]) or
               (rankList[2] = rankList[4]) or
               (rankList[3] = rankList[5]);
    
    end isTOAK;



    function isTP(h : Hand) return Boolean is
        rankList : Integer_Array := getRankList(h);
    begin

        return (rankList[1] = rankList[2] and rankList[3] = rankList[4]) or
		       (rankList[1] = rankList[2] and rankList[4] = rankList[5]) or
		       (rankList[2] = rankList[3] and rankList[4] = rankList[5]);
    
    end isTP;



    --  function isP(h : Hand) return Boolean is
    -- rankList : Integer_Array := getRankList(h);
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



    procedure assessHand(h : in out Hand) is
        jokerIndex1 : Integer := -1;
        jokerIndex2 : Integer := -1;

        jokerSuit1 : Integer := 0;
        jokerSuit2 : Integer := 0;

        availCards1 : Card_Array(1..52);
        availCards2 : Card_Array(1..52);

        topScore : Integer := 0;
        currScore : Integer := 0;
        topSorted : Card_Array(1..5);
        
    begin

        for i in h.cards'Range loop
            if (h.cards(i).rank = 15) then
                jokerIndex1 := i;
                jokerSuit1 := h.cards(i).suit;
                availCards1 := h.generateAvailableCards(jokerSuit1);
            else
                jokerIndex2 := i;
                jokerSuit2 := h.cards(i).suit;
                availCards2 := h.generateAvailableCards(jokerSuit2);
                exit;
            end if;
        end loop;

        if (jokerIndex1 /= -1) then
            for i in availCards1'Range loop
                for j in availCards2'Range loop
                    h.cards(jokerIndex1) := Init_Card(h.cards(i).rank, h.cards(i).suit);

                    if (jokerIndex2 /= -1) then
                        h.cards(jokerIndex2) := Init_Card(h.cards(j).rank, h.cards(j).suit);
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



    procedure Add_Card(h : in out Hand; c : Card.Card) is
    begin

        for i in h.cards'Range loop
            if (h.cards(i).rank = -1) then
                h.cards(i) := Init_Card(c.rank, c.suit);
                exit;

            end if;
        end loop;

    end Add_Card;



    procedure sortHand(h: in out Hand) is

       Temp: Card.Card;

    begin
    
       for I in h.Cards'Range loop
          h.Sorted(I) := Init_Card(h.Cards(I).rank, h.Cards(I).suit);
       end loop;
    
       for J in 1 .. 4 loop
          for I in 1 .. 4 loop
             if Compare_Card(h.Sorted(I), h.Sorted(I + 1)) > 0 then 
                Temp := Init_Card(h.Sorted(I + 1).rank, h.Sorted(I + 1).suit);
                h.Sorted(I + 1) := h.Sorted(I);
                h.Sorted(I) := Temp;
             end if;
          end loop;
       end loop;

    end sortHand;


end Hand;