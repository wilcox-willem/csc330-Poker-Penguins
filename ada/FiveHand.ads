-- FiveHand.ads

with Card;
with Hand;

package FiveHand is

   type FiveHand is private;

   function Init_Five_Hand(File : String; J_Flag : Boolean) return FiveHand;
   procedure Play(File : String; J_Flag : Boolean);
   procedure PlayStats(S_Count : Integer; J_Flag : Boolean);
   procedure Draw_Cards(Game_Type : Integer);
   procedure Print_All_Hands;
   procedure Sort_Hands;
   function Hands_Stats(F: Five_Hand) return Integer_Array;

private
   type FiveHand is record
      Deck : Card.Deck;
      Hands : array(1..6) of Hand.Hand;
      J_Flag : Boolean;
      S_Flag : Boolean;
      S_Count : Integer;
   end record;

end FiveHand;

