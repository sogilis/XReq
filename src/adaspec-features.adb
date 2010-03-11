--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Util.IO;

use Ada.Strings.Fixed;

package body AdaSpec.Features is

   --------------------------
   --  Scenario  --  Make  --
   --------------------------

   procedure Make   (Scenario : out    Scenario_Type;
                     Name     : in     String := "")
   is
      S : Scenario_Type := Null_Scenario;
   begin
      S.Name   := To_Unbounded_String (Name);
      Scenario := S;
   end Make;

   ----------------------------
   --  Scenario  --  Append  --
   ----------------------------

   procedure Append (Scenario : in out Scenario_Type;
                     Stanza   : in     Stanza_Type)
   is
      use Stanza_Container;
   begin
      Append (Scenario.Stanzas, Stanza);
   end Append;

   ------------------------------
   --  Feature_Type  --  Make  --
   ------------------------------

   procedure Make (F      : out    Feature_Type;
                   Name   : in     String := "")
   is
      Feature : Feature_Type := Null_Feature;
   begin
      Feature.Name := To_Unbounded_String (Name);
      F := Feature;
   end Make;

   --------------------------------
   --  Feature_Type  --  Parsed  --
   --------------------------------

   function Parsed (F : in Feature_Type) return Boolean is
      pragma Unreferenced (F);
   begin
      return True;
   end Parsed;

   -------------------------------------
   --  Feature_File_Type  --  Parsed  --
   -------------------------------------

   function Parsed (F : in Feature_File_Type) return Boolean is
   begin
      return F.Parsed;
   end Parsed;

   ------------------------------
   --  Feature_Type  --  Name  --
   ------------------------------

   function Name (F : in Feature_Type) return String is
   begin
      return To_String (F.Name);
   end Name;

   ------------------------------------------------
   --  Feature_File_Type  --  Null_Feature_File  --
   ------------------------------------------------

   function Null_Feature_File return Feature_File_Type is
      F : Feature_File_Type;
   begin
      return F;
   end Null_Feature_File;

   -----------------------------------
   --  Feature_File_Type  --  Make  --
   -----------------------------------

   procedure Make (F         : out Feature_File_Type;
                   File_Name : in  String)
   is
      Feature : Feature_File_Type := Null_Feature_File;
   begin
      Feature.File_Name := To_Unbounded_String (File_Name);
      Feature.Parsed    := False;
      F := Feature;
   end Make;

   -------------------------------------
   --  Feature_File_Type  --  Create  --
   -------------------------------------

   function  Create    (File_Name : in     String) return Feature_File_Type is
      F : Feature_File_Type;
   begin
      Make (F, File_Name);
      return F;
   end Create;


   ------------------------------------
   --  Feature_File_Type  --  Parse  --
   ------------------------------------

   procedure Parse (F      : in out Feature_File_Type;
                    Errors : out Boolean) is

      Self : constant access Feature_File_Type'Class := F'Access;

      use Ada.Text_IO;
      use Util.IO;
      use Util.Strings;
      use Util.Strings.Vectors;
      use Scenario_Container;
      use Stanza_Container;

      type Mode_Type is (M_Begin, M_Feature, M_Background, M_Scenario, M_Str);

      --  The keywords
      K_Feature        : constant String := "Feature:";
      K_Background     : constant String := "Background:";
      K_Scenario       : constant String := "Scenario:";
      K_Given          : constant String := "Given ";
      K_When           : constant String := "When ";
      K_Then           : constant String := "Then ";
      K_And            : constant String := "And ";
      K_StrDouble      : constant String := """""""";
      K_StrSimple      : constant String := "'''";

      --  State machine
      Curr_State       : Mode_Type         := M_Begin;     --  Current State
      State_Saved      : Mode_Type;                        --  Previous State
      Stanza_State     : Prefix_Type_Maybe := Prefix_None; --  Kind of step

      --  General purpose variables
      File             : File_Type;         --  Current file
      Position         : Position_Type;     --  Position in file
      Line_S           : Unbounded_String;  --  Current line
      Idx_Start        : Natural;           --  First non blank character index


      --  Current stanza
      Current_Stanza   : Stanza_Type;
      Has_Stanza       : Boolean := False;

      --  Current multi-line argument
      CurrentStr       : String (1 .. 3);       --  Long string type """ or '''

      Suffix           : Unbounded_String;
      Long_String      : Unbounded_String;
      Idx_Start_Str    : Natural;

      -------------------------------------------------
      --  GENERAL INFORMATION ON THE STATE MACHINES  --
      -------------------------------------------------
      --  Parsing is line oriented, on each line, a transition on the current
      --  state machine is triggered.
      --
      --  The state machine gather information about the current block geing
      --  parsed.

      -----------------------------------------
      --  FINITE STATE MACHINE  --  LEVEL 0  --
      -----------------------------------------

      type Command_Type is (Cmd_Reset, Cmd_Start, Cmd_Run);

      type State_0_Type is (S0_Init, S0_Feature, S0_Background, S0_Scenario);
      type State_1_Type renames Prefix_Type_Maybe;
      type State_2_Type is (S2_Init, S2_String, S2_Table);

      State_0 : State_0_Type := S0_Init;
      State_1 : State_1_Type := Prefix_None;
      State_2 : State_2_Type := S2_Init;

      procedure FSM_Level_0 is
         New_Scenario : Boolean := False;
      begin
         case State_0 is
            when S0_Init =>

               --  Found: "Feature: Name"
               if Starts_With_K (K_Feature) then
                  State_0   := S0_Feature;
                  Self.Name := Trimed_Suffix (Line_S,
                                              Idx_Start + K_Feature'Length);
               end if;

            when S0_Feature | S0_Background | S0_Scenario =>

               --  Found "Background:"
               if Starts_With_K (K_Background) then
                  New_Scenario := True;
                  State_O      := S0_Background;
                  Idx_Start    := Idx_Start + K_Background'Length;

               --  Found "Scenario: Name"
               elsif Starts_With_K (K_Scenario) then
                  New_Scenario := True;
                  State_O      := S0_Scenario;
                  Idx_Start    := Idx_Start + K_Scenario'Length;
               end if;

               if New_Scenario then
                  

               --  In a scenario, go to level 1
               elsif State_0 /= S0_Feature then
                  FSM_Level_1;
               end if;

         end case;
      end FSM_Level_0;

      procedure FSM_Level_1 is
      begin
         case State_1 is
            when Prefix_None =>

            when Prefix_Given | Prefix_When | Prefix_Then =>

               --  Found "Background:"
               if Starts_With_K (K_Background) then
                  New_Scenario (K_Background, M_Background);

               --  Found "Scenario: Name"
               elsif Starts_With_K (K_Scenario) then
                  New_Scenario (K_Scenario, M_Scenario);

               --  In a scenario, go to level 1
               elsif State_0 /= S0_Feature then
                  FSM_Level_1;
               end if;

         end case;
      end FSM_Level_1;

      ------------------------------------------------
      --  GENERAL INFORMATION ON THE STATE MACHINE  --
      ------------------------------------------------
      --  For each state, there are generally two procedures:
      --  New_<Element>:
      --      This procedure is called when entering the state. It initialize
      --      the state variables and the current element.
      --  Add_<Element>:
      --      This procedure is called when another element of the same
      --      hierarchy level starts. It takes the current element variables 
      --      and save them in the current element object.
      --      Then it reset the variables it used for a new element
      --      This is generally called in a New_<Element> procedure
      --
      --  There are sub state machines for each scenarios. When in a scenario,
      --  the `Stanza_State' variable holds the state of the current scenario.
      --
      --  When parsing a multi line argument (long string for example) the 
      --  state machine exit the current 

      procedure New_Feature;
      procedure New_Scenario (Keyword : in String; Typ : in Mode_Type);
      procedure Add_Scenario;
      procedure Add_Stanza;
      procedure Add_Text;

      ------------
      --  INIT  --
      ------------
      --  This is the initialization state.
      --  Going to FEATURE when we find the Feature keyword
      --  Scenarios are not allowed at top level

      ---------------
      --  FEATURE  --
      ---------------
      --  This is when the feature starts, when no scenarious have been found
      --  In this, the lines of text are added to the feature description
      --  When a scenario is found, go to the SCENARIO state

      procedure New_Feature is
      begin
         State_0      := S0_Feature;
         Curr_State   := M_Feature;
         Stanza_State := Prefix_None;
         Self.Name    := Trimed_Suffix (Line_S, Idx_Start + K_Feature'Length);
      end New_Feature;

      -----------------------------
      --  BACKGROUND / SCENARIO  --
      -----------------------------
      --  This state is entereg when a scenario keyword is found and we are in
      --  the SCENARIO or FEATURE state.
      --  This state is not left until

      Current_Scenario : Scenario_Type;

      procedure New_Scenario (Keyword : in String; Typ : in Mode_Type) is
      begin
         Add_Scenario;
         Curr_State := Typ;
         Stanza_State := Prefix_None;
         Current_Scenario.Name :=
            Trimed_Suffix (Line_S, Idx_Start + Keyword'Length);
      end New_Scenario;

      procedure Add_Scenario is
      begin
         Add_Stanza;
         if Curr_State = M_Scenario then
            Append (Self.Scenarios, Current_Scenario);
         elsif Curr_State = M_Background then
            Self.Background.Stanzas := Current_Scenario.Stanzas;
         end if;
         Current_Scenario.Name := Null_Unbounded_String;
         Clear (Current_Scenario.Stanzas);
      end Add_Scenario;

      --  Called when a step or a scanario starts. This saves the previous step
      --  in the current scenario.
      procedure Add_Stanza is
      begin
         if Has_Stanza then
            Append (Current_Scenario.Stanzas, Current_Stanza);
            Has_Stanza     := False;
         end if;
         Current_Stanza := Null_Stanza;
      end Add_Stanza;

      --  Called when a new multi-line element, a step or a scenario starts. 
      --  This saves the current multi-line argument in the current step
      procedure Add_Text is
      begin
         Append (Current_Stanza.Texts, Long_String);
         Long_String := Null_Unbounded_String;
      exception
         --  GCOV_IGNORE_BEGIN
         when Constraint_Error =>
            Put_Line ("Error: long string error: """"""" &
                      To_String (Long_String) & """""""");
            Long_String := Null_Unbounded_String;
         --  GCOV_IGNORE_END
      end Add_Text;


      --  Return true if the current line starts with the keyword `Keyword'
      function Starts_With_K (Keyword : in String) return Boolean;
      function Starts_With_K (Keyword : in String) return Boolean is
      begin
         return Starts_With (To_String (Line_S), Keyword, Idx_Start);
      end Starts_With_K;

   begin
      Errors := False;
      Position := Position_Type'(
         File => Self.File_Name,
         Line => 1);
      Open (File, In_File, To_String (Self.File_Name));
      while not End_Of_File (File) loop
         --
         --  Read Line
         --
         Line_S    := Get_Whole_Line (File);
         Idx_Start := Index_Non_Blank (Line_S);

         --
         --  State Machine
         --
         case Curr_State is
            when M_Begin =>

               --  Found: "Feature: Name"
               if Starts_With_K (K_Feature) then
                  Curr_State := M_Feature;
                  Stanza_State := Prefix_None;
                  Self.Name := Trimed_Suffix (Line_S,
                                           Idx_Start + K_Feature'Length);
               end if;

            when M_Feature =>

               --  Found "Background:"
               if Starts_With_K (K_Background) then
                  New_Scenario (K_Background, M_Background);

               --  Found "Scenario: Name"
               elsif Starts_With_K (K_Scenario) then
                  New_Scenario (K_Scenario, M_Scenario);
               end if;

            when M_Background | M_Scenario =>

               --  Found "Background:"
               if Starts_With_K (K_Background) then
                  New_Scenario (K_Background, M_Background);

               --  Found "Scenario: Name"
               elsif Starts_With_K (K_Scenario) then
                  New_Scenario (K_Scenario, M_Scenario);

               --  Found "Given ..."
               elsif Starts_With_K (K_Given) then
                  Add_Stanza;
                  Has_Stanza := True;
                  Stanza_State := Prefix_Given;
                  Current_Stanza.Pos := Position;
                  Idx_Start := Idx_Start + K_Given'Length;

               --  Found "When ..."
               elsif Starts_With_K (K_When) then
                  Add_Stanza;
                  Has_Stanza := True;
                  Stanza_State := Prefix_When;
                  Current_Stanza.Pos := Position;
                  Idx_Start := Idx_Start + K_When'Length;

               --  Found "Then ..."
               elsif Starts_With_K (K_Then) then
                  Add_Stanza;
                  Has_Stanza := True;
                  Stanza_State := Prefix_Then;
                  Current_Stanza.Pos := Position;
                  Idx_Start := Idx_Start + K_Then'Length;

               --  Found "And ..."
               elsif Starts_With_K (K_And) then
                  Add_Stanza;
                  Has_Stanza := True;
                  Current_Stanza.Pos := Position;
                  Idx_Start := Idx_Start + K_And'Length;

               --  Found """
               elsif Starts_With_K (K_StrDouble) then
                  State_Saved  := Curr_State;
                  Curr_State         := M_Str;
                  CurrentStr    := K_StrDouble;
                  Idx_Start_Str := Idx_Start;
                  Idx_Start     := Idx_Start + CurrentStr'Length;

               --  Found '''
               elsif Starts_With_K (K_StrSimple) then
                  State_Saved   := Curr_State;
                  Curr_State         := M_Str;
                  CurrentStr    := K_StrSimple;
                  Idx_Start_Str := Idx_Start;
                  Idx_Start     := Idx_Start + CurrentStr'Length;
               end if;

               --  Record first line of long string
               if Curr_State = M_Str then
                  if Index_Non_Blank (Line_S, Idx_Start) /= 0 then
                     Put_Line ("Error: PyString: found text after " &
                               CurrentStr);
                     Errors := True;
                     Curr_State  := State_Saved;
                  end if;

               --  Continue the stanza on the next line
               elsif Stanza_State /= Prefix_None then
                  Current_Stanza.Prefix := Stanza_State;
                  Suffix := Trimed_Suffix (Line_S, Idx_Start);
                  if Current_Stanza.Stanza /= Null_Unbounded_String then
                     if Suffix /= Null_Unbounded_String then
                        Append (Current_Stanza.Stanza, " ");
                     end if;
                  end if;
                  if Suffix /= Null_Unbounded_String then
                     Append (Current_Stanza.Stanza, Suffix);
                  else
                     Add_Stanza;
                  end if;
               end if;

            when M_Str =>

               declare
                  I    : Natural;
                  Line : constant String := To_String (Line_S);
               begin
                  if Starts_With_K (CurrentStr) then
                     Idx_Start := Idx_Start + CurrentStr'Length;
                     if Index_Non_Blank (Line, Idx_Start) = 0 then
                        Curr_State := State_Saved;
                        --  End of string, remove the last line feed.
                        Head (Long_String, Length (Long_String) - 1);
                        Add_Text;
                     end if;
                  end if;

                  if Curr_State = M_Str then
                     I := Natural'Min (Index_Non_Blank (Line), Idx_Start_Str);
                     I := Natural'Max (I, 1);
                     while I <= Line'Last loop
                        if I <= Line'Last - 1 and then
                           Line (I .. I + 1) = "\" & CurrentStr (1)
                        then
                           Append (Long_String, CurrentStr (1));
                           I := I + 2;
                        else
                           Append (Long_String, Line (I));
                           I := I + 1;
                        end if;
                     end loop;
                     Append (Long_String, ASCII.LF);
                  end if;
               end;

         end case;

         Position.Line := Position.Line + 1;
      end loop;
      Add_Scenario;
      Close (File);
      Self.Parsed := True;
   end Parse;

   ----------------------------------------
   --  Feature_File_Type  --  File_Name  --
   ----------------------------------------

   function  File_Name (F : in Feature_File_Type) return String is
   begin
      return To_String (F.File_Name);
   end File_Name;


   ----------------------------------------
   --  Feature_File_Type  --  To_String  --
   ----------------------------------------

   function To_String (F : in Feature_File_Type) return String is
      CRLF : constant String := ASCII.CR & ASCII.LF;
   begin
      return "# File: " & To_String (F.File_Name) & CRLF &
             To_String (Feature_Type (F));
   end To_String;


   -----------------------------------
   --  Feature_Type  --  To_String  --
   -----------------------------------

   function To_String (F : in Feature_Type) return String is

      Self : constant access constant Feature_Type'Class := F'Access;

      use Scenario_Container;
      CRLF : constant String := ASCII.CR & ASCII.LF;
      Res  : Unbounded_String;
      Cur  : Scenario_Container.Cursor := First (F.Scenarios);
      Sce  : Scenario_Type;

      procedure Output_Stanzas (V : in Stanza_Container.Vector);
      procedure Output_Stanzas (V : in Stanza_Container.Vector) is
         use Stanza_Container;
         Pre  : Prefix_Type_Maybe := Prefix_None;
         Cur  : Stanza_Container.Cursor := First (V);
         Sta  : Stanza_Type;
      begin
         while Has_Element (Cur) loop
            Append (Res, "    ");
            Sta := Element (Cur);
            if Sta.Prefix = Pre then
               Append (Res, "And");
            else
               case Sta.Prefix is
                  when Prefix_Given => Append (Res, "Given");
                  when Prefix_When =>  Append (Res, "When");
                  when Prefix_Then =>  Append (Res, "Then");
               end case;
               Pre := Sta.Prefix;
            end if;
            Append (Res, " ");
            Append (Res, To_String (Sta.Stanza));
            Append (Res, CRLF);
            Next (Cur);
         end loop;
      end Output_Stanzas;

   begin
      if not Parsed (Self.all) then
         raise Unparsed_Feature;
      end if;
      Append (Res, "Feature: " & To_String (F.Name) & CRLF);
      Append (Res, CRLF);
      Append (Res, "  Background: " & To_String (F.Background.Name) & CRLF);
      Output_Stanzas (F.Background.Stanzas);
      Append (Res, CRLF);
      while Has_Element (Cur) loop
         Sce := Element (Cur);
         Append (Res, "  Scenario: " & To_String (Sce.Name) & CRLF);
         Output_Stanzas (Sce.Stanzas);
         Append (Res, CRLF);
         Next (Cur);
      end loop;
      return To_String (Res);
   end To_String;

   --------------------------------
   --  Feature_Type  --  Append  --
   --------------------------------

   procedure Append         (F      : in out Feature_Type;
                             S      : in     Scenario_Type)
   is
      use Scenario_Container;
   begin
      Append (F.Scenarios, S);
   end Append;

   ----------------------------------------
   --  Feature_Type  --  Set_Background  --
   ----------------------------------------

   procedure Set_Background (F      : in out Feature_Type;
                             Bg     : in     Scenario_Type)
   is
   begin
      F.Background := Bg;
   end Set_Background;

end AdaSpec.Features;
