package body Hand is

    function Init_Hand return Hand is

    begin

    return null;
    
    end Init_Hand;



    function To_String(h : Hand) return String is

    begin

    return null;
    
    end To_String;



    function Compare(hand1, hand2 : Hand) return Integer is

    begin

    return null;
    
    end Compare;



    function CompareHelper(hand1, hand2 : Hand; diff : Integer; pass : Integer) return Integer is

    begin

    return null;
    
    end CompareHelper;



    function isRSF(h : Hand) return Boolean is

    begin

    return null;
    
    end isRSF;



    function isSF(h : Hand) return Boolean is

    begin

    return null;
    
    end isSF;



    function isFOAK(h : Hand) return Boolean is

    begin

    return null;
    
    end isFOAK;



    function isFH(h : Hand) return Boolean is

    begin

    return null;
    
    end isFH;



    function isF(h : Hand) return Boolean is

    begin

    return null;
    
    end isF;



    function isS(h : Hand) return Boolean is

    begin

    return null;
    
    end isS;



    function isTOAK(h : Hand) return Boolean is

    begin

    return null;
    
    end isTOAK;



    function isTP(h : Hand) return Boolean is

    begin

    return null;
    
    end isTP;



    function isP(h : Hand) return Boolean is

    begin

    return null;
    
    end isP;



    function getTieBreakerCard(h : Hand; pass : Integer) return Card is

    begin

    return null;
    
    end getTieBreakerCard;



    function getRankList(h : Hand) return Integer_Array is

    begin

    return null;
    
    end getRankList;



    function getKicker(h : Hand) return Card is

    begin

    return null;
    
    end getKicker;



    --  function generateAvailableCards(h : Hand; color : Integer) is

    --  begin

    --  return null;
    
    --  end generateAvailableCards;



    function contains(arr : Card_Array; rank : Integer; suit : Integer) return Boolean is

    begin

    return null;

    end contains;



    --  procedure assessHand(h : Hand) is

    --  begin

    --  end assessHand;



    --  procedure Add_Card(h : Hand; card : Card) is

    --  begin

    --  end Add_Card;



    --  procedure sortHand(h : Hand) is

    --  begin

    --  end sortHand;

end Hand;