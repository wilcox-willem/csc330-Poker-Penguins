with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Card; use Card;
with Hand; use Hand;

package body FiveHand is

    -- Represents the game of Five Hand, a poker game with 6 hands.
    -- The game can be played with a randomized deck or a deck loaded from a file.

    -- rough draft from Go
    -- function names from Go
    -- STILL NEEDS IMPLEMENTING
    function Init_Five_Hand (file : String; j_Flag : Boolean) return FiveHand is
        d : Deck := Init_Deck();
        h : Hands -- hand array;

    begin
       -- Initialize 6 hands inside the hand array
       for I in 1..6 loop
          h(I) := Init_Hand;
       end loop;
       
       -- Build the deck based on command line arguments
       if File /= "" then
          Deck.Build_File_Deck(d, file);
       else
          Deck.Build_Rand_Deck(d, j_Flag);
       end if; 

       return (Deck => d, Hands => h, j_Flag => j_Flag, Others => ());

    end Init_Five_Hand;

    -- NEEDS IMPLEMENTING
    function play(file : String; j_Flag : Boolean);
    
    begin
    
    end play;
    
    -- NEEDS IMPLEMENTING
    function playStats(s_Count : Integer; j_Flag : Boolean);
    Hand_Stat_List : array(1..10) of Integer := (others => 0);
    Hand_Titles : constant array(1..10) of String := (
        "HIGH CARD",
        "PAIR", 
        "TWO PAIR",
        "THREE OF A KIND",
        "STRAIGHT",
        "FLUSH",
        "FULL HOUSE",
        "FOUR OF A KIND",
        "STRAIGHT FLUSH",
        "ROYAL STRAIGHT FLUSH"
    );
    begin
        Put_Line("\n---- STATISTICAL ANALYSIS ----");
    
        for I in 1..s_Count loop
            declare
                Game : Five_Hand := Init_Five_Hand("", j_Flag);
                Player_Scores : array(1..6) of Integer;
            begin
                Draw_Cards(Game, 0);
                Player_Scores := hands_Stats(Game);
    
                for J in 1..Player_Scores'Length loop
                    declare
                        Score_Index : Integer := Player_Scores(J) - 1;
                    begin
                        Hand_Stat_List(Score_Index + 1) := Hand_Stat_List(Score_Index + 1) + 1;
                    end;
                end loop;
            end;
        end loop;
    
        for I in reverse Hand_Titles'Range loop
            Put_Line(Hand_Titles(I) & String'(" ") & Integer'Image(Hand_Stat_List(I)));
        end loop;
    
        Put_Line;
    end playStats;
    
    
    -- NEEDS IMPLEMENTING
    function Hands_Stats(F : Five_Hand) return Integer_Array is
       Player_Scores : Integer_array(1..6);
    begin
       for I in F.Hands'Range loop
          AssessHand(F.Hands(I));
          Player_Scores(I) := F.Hands(I).handType;
       end loop;
    
       return Player_Scores;
    end Hands_Stats;
    
    -- NEEDS IMPLEMENTING
    function draw_Cards(game_Type : Integer) is 
    
    begin

    end draw_Cards;
    
    -- NEEDS IMPLEMENTING
    function print_All_Hands()
    
    begin

    end print_All_Hands;
    
    -- NEEDS IMPLEMENTING
    function sort_Hands() is 
    
    begin

    end sort_Hands;
    


end FiveHand;

-- main attempt




--------------------------------------------------------

-- CommandLineArgs.adb
function Main_program ()

with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;

procedure Main_Program is
    fileName : Unbounded_String;
    j_Flag : Boolean := False;
    s_Flag : Boolean := False;
    s_Count : Integer := 1000;
begin

    -- Check if there are any command line arguments
    if Ada.Command_Line.Argument_Count > 0 then
        for I in 1 .. Ada.Command_Line.Argument_Count loop
            --Ada.Text_IO.Put_Line("Argument " & Integer'Image(I) & ": " & Ada.Command_Line.Argument(I));
            if Ada.Command_Line.Argument(I) = "-j" then
                j_Flag := True; 
            elsif Ada.Command_Line.Argument(I) = "-s" then 
                s_Flag := True;
                if Ada.Command_Line.Argument(I + 1) /= Null_Unbounded_String then
                    if Ada.Command_Line.Argument(I) /= "-j" then
                      s_Count := Ada.Command_Line.Argument(I + 1);
                    end if;
                end if;
            elsif Ada.Command_Line.Argument(I) = "-f" then
                fileName := Ada.Command_Line.Argument(I + 1);
            end if;
        end loop;   
    end if;
   

    -- WW: needs adjusting when FH.abd is further along
    if s_Flag then 
        playStats(s_Count, j_Flag);
    else
        game := Init_Five_Hand(f, j_Flag);
        game.play();
      
    end if;
   
end Main_Program;
