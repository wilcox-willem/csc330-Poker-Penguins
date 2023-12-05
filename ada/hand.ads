with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Card; use Card;

package Hand is
    type Card_Array is array (Positive range <>) of Card.Card;
    type Integer_Array is array(Positive range <>) of Integer;

    type Hand is record

        cards  : Card_Array(1..5);
        sorted : Card_Array(1..5);
        handType : Integer;

    end record;

    function Init_Hand return Hand;
    function Hand_To_String(h : Hand) return String;
    function Compare_Hand(hand1, hand2 : in out Hand) return Integer;
    function CompareHelper(hand1, hand2 :  in out Hand; diff : Integer; pass : Integer) return Integer;
    --  function isRSF(h : Hand) return Boolean;
    --  function isSF(h : Hand) return Boolean;
    --  function isFOAK(h : Hand) return Boolean;
    --  function isFH(h : Hand) return Boolean;
    --  function isF(h : Hand) return Boolean;
    --  function isS(h : Hand) return Boolean;
    --  function isTOAK(h : Hand) return Boolean;
    --  function isTP(h : Hand) return Boolean;
    --  function isP(h : Hand) return Boolean;
    function getTieBreakerCard(h : Hand; pass : Integer) return Card.Card;
    function getRankList(h : Hand) return Integer_Array;
    function getKicker(h : Hand) return Card.Card;
    --  function generateAvailableCards(h : Hand; color : Integer) return Card_Array;
    --  function contains(arr : Card_Array; rank : Integer; suit : Integer) return Boolean;

    procedure assessHand(h : in out Hand);
    procedure Add_Card(h : in out Hand; c : Card.Card);
    procedure sortHand(h : in out Hand);


end Hand;