--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Containers.Vectors;

use Ada.Text_IO;
use Ada.Strings.Fixed;

package body AdaSpec_Feature_Parser is

   --  Keywords
   K_Feature          :         constant String := "Feature:";
   K_Background       :         constant String := "Background:";
   K_Scenario         :         constant String := "Scenario:";
   K_Scenario_Outline :         constant String := "Scenario Outline:";
   K_Examples         :         constant String := "Examples:";
   K_Given            : limited constant String := "Given ";
   K_When             : limited constant String := "When ";
   K_Then             : limited constant String := "Then ";
   K_And              :         constant String := "And ";
   K_StrDouble        :         constant String := """""""";
   K_StrSimple        :         constant String := "'''";

   package String_Vectors is
      new Ada.Containers.Vectors (Natural, Unbounded_String, "=");
   subtype String_Vector is Vectors.Vector;


   function To_String_Array (Vec : in String_Vector) is
      use String_Vectors;
      Arr : String_Array (0 .. Length (Vec) - 1);
   begin
      for I in Desc'Range loop
         Desc (I) := Element (Vec, I);
      end loop;
      return Arr;
   end To_String_Array;


   procedure Parse (Feature   : in out Feature_Type;
                    File_Name : in     String)
   is

      procedure Log_Error (Error : in String);
      procedure Parser_Read_Line;
      function  Parser_End_Of_File return Boolean;
      function  Detect_Keyword (Keyword : in String) return Boolean;

      procedure Read_All;
      procedure Read_Keywords;
      procedure Read_Feature;
      procedure Read_Scenario (Scenario : out    Scenario_Type;
                               Outline  : in     Boolean := False);
      procedure Read_Step     (Step     : in out Step_Type);
      procedure Read_String   (Result   : out    Unbounded_String;
                               Sep      : in     String);
      procedure Read_Table;

      --  Context variables
      Position_Line   : Natural := 0;     --  Current line number
      Line_S          : Unbounded_String; --  Current line
      Idx_Start       : Natural;          --  First non blank on current line
      Idx_Data        : Natural;          --  Index where data starts
      --                                  --  (after the keyword generally)
      Unread_Line     : Boolean := False; --  Unread the line;
      Current_Tags    : String_Vector;

      -------------------------
      --  Utility Functions  --
      -------------------------

      procedure Log_Error (Error : in String)
      is
      begin
         Log_Line (Error & " in " & File_Name & " line" &
                   Position_Line'Img);
      end Log_Error;

      procedure Parser_Read_Line is
      begin
         if Unread_Line then
            Unread_Line := False;
         else
            Position_Line := Position_Line + 1;
            Line_S        := To_Unbounded_String (Read_Line);
            Idx_Start     := Index_Non_Blank (Line_S);
            Idx_Data      := Idx_Start;
         end if;
      end Parser_Read_Line;

      function  Parser_End_Of_File return Boolean is
      begin
         return not Unread_Line and then End_Of_File;
      end Parser_End_Of_File;

      function  Detect_Keyword (Keyword : in String) return Boolean is
         Detect : Boolean;
      begin
         Detect := Starts_With (To_String (Line_S), Keyword, Idx_Start);
         if Detect then
            Idx_Data := Idx_Start + Keyword'Length;
         end if;
         return Detect;
      end Detect_Keyword;

      ---------------------
      --  Parsing Rules  --
      ---------------------

      --  ++ ALL           -> FEATURE
      procedure Read_All is
      begin
         Parser_Read_Line;
         if Detect_Keyword (K_Feature) then
            Read_Feature;
         end if;
      end Read_All;

      --  ++  KEYWORDS     ->  { @[^\r\n\t ]+ }
      procedure Read_Keywords is
         Buffer     : Unbounded_String;
         Line       : constant String := To_String (Line_S);
         In_Keyword : Boolean := False;
      begin
         for I in Line'Range loop
            case Line (I) is
               when ASCII.CR | ASCII.LF | ASCII.HT | ' ' =>
                  if In_Keyword then
                     Append (Current_Tags, Buffer);
                     In_Keyword := False;
                  end if;
               when '@' =>
                  if not In_Keyword then
                     In_Keyword := True;
                     Buffer     := Null_Unbounded_String;
                  end if;
                  Append (Buffer, Line (I));
               when others =>
                  Append (Buffer, Line (I));
            end case;
         end loop;
         if In_Keyword then
            Append (Current_Tags, Buffer);
         end if;
      end Read_Keywords;

      --  ++ FEATURE       -> "Feature:" name NL
      --  ++                   description NL
      --  ++                   { SCENARIO }
      procedure Read_Feature is
         use String_Vectors;
         Current_Scenario : Scenario_Type;
         Beginning        : Boolean := True;
         Had_Description  : Boolean := False;
         Data             : Unbounded_String;
         Description      : String_Vector;
         I                : String_Vectors.Cursor;
      begin
         Set_Name     (Feature, Trimed_Suffix (Line_S, Idx_Data));
         Set_Position (Feature, Position_Line, File_Name);
         while not Parser_End_Of_File loop
            Parser_Read_Line;
            if Detect_Keyword (K_Background) then
               Create (Current_Scenario);
               Read_Scenario (Current_Scenario);
               Beginning := False;
            elsif Detect_Keyword (K_Scenario) then
               Create (Current_Scenario);
               Read_Scenario (Current_Scenario);
               Append_Scenario (Feature, Current_Scenario);
               Beginning := False;
            elsif Detect_Keyword (K_Scenario_Outline) then
               Create (Current_Scenario);
               Read_Scenario (Current_Scenario, True);
               Append_Scenario (Feature, Current_Scenario);
               Beginning := False;
            elsif Detect_Keyword ("#") then
               null;
            elsif Detect_Keyword ("@") then
               Read_Keywords;
            elsif Beginning then
               Data := Trimed_Suffix (Line_S, Idx_Data);
               if Data /= Null_Unbounded_String or Had_Description then
                  Append (Description, Data);
                  Had_Description := True;
               end if;
            end if;
         end loop;
         --  Clean up desription
         I := Last (Description);
         while Has_Element (I) and then
               Element (I) = Null_Unbounded_String
         loop
            Delete_Last (Description);
            I := Last (Description);
         end loop;
         --  Hooks
         Set_Description (Feature, To_String_Array (Description));
      end Read_Feature;

      --  ++ SCENARIO      -> K_SCENARIO name NL
      --  ++                  { STANZA }
      --  ++                  [ SCENARIO_EX ]
      --  ++ K_SCENARIO    -> "Background:"
      --  ++                | "Scenario:"
      --  ++                | "Scenario Outline:"
      --  ++ SCENARIO_EX   -> "Examples:" NL
      --  ++                  TABLE
      procedure Read_Scenario (Scenario : out Scenario_Type;
                               Outline  : in  Boolean := False) is
         Current_Step   : Step_Type;
         Current_Prefix : String'Access := null;
         Detect         : Boolean;
         Continue       : Boolean := True;
      begin
         Scenario := Get_Scenario
            (Outline     => Outline,
             Name        => Trimed_Suffix (Line_S, Idx_Data),
             Line_Number => Position_Line,
             File_Name   => File_Name,
             Tags        => To_String_Array (Tags));
         Clear (Current_Tags);
         while Continue and not Parser_End_Of_File loop

            Parser_Read_Line;
            Detect := True;

            if Detect_Keyword (K_Background)       or else
               Detect_Keyword (K_Scenario)         or else
               Detect_Keyword (K_Scenario_Outline) or else
               Detect_Keyword ("@")
            then
               Unread_Line := True;
               Continue    := False;
               Detect      := False;
            elsif Detect_Keyword ("#") then
               null;
            elsif Outline and then Detect_Keyword (K_Examples) then
               Parser_Read_Line;
               if not Parser_End_Of_File then
                  Read_Table;
               end if;
               Detect := False;
            elsif Detect_Keyword (K_Given) then
               Current_Prefix := K_Given'Access;
               Current_Prefix := True;
            elsif Detect_Keyword (K_When) then
               Current_Prefix := (K_When'Access;
               Current_Prefix := True;
            elsif Detect_Keyword (K_Then) then
               Current_Prefix := (K_Then)'Access;
               Current_Prefix := True;
            elsif Detect_Keyword (K_And) then
               if Current_Prefix = null then
                  Log_Error    ("ERROR: And keyword");
                  Log_Line ("       And keyword should be following " &
                                   "another keyword");
                  Log_Line ("       Ignoring step");
               end if;
            elsif Trimed_Suffix (Line_S, Idx_Data) /= "" then
               Log_Error    ("ERROR: invalid format");
               raise Parse_Error;
            else
               Detect := False;
            end if;

            if Detect then
               if    Current_Prefix = K_Given'Access then
                  Create_Given (Current_Step);
               elsif Current_Prefix = K_When'Access then
                  Create_When (Current_Step);
               elsif Current_Prefix = K_Then'Access then
                  Create_Then (Current_Step);
               end if;
               Read_Step (Current_Step);
               if Current_Prefix /= null then
                  Append_Step (Scenario, Current_Step);
               end if;
            end if;

         end loop;
      end Read_Scenario;

      --  ++ STANZA        -> K_STANZA text NL
      --  ++                  { STANZA_PARAM }
      --  ++ K_STANZA      -> "Given"
      --  ++                | "When"
      --  ++                | "Then"
      --  ++                | "And"
      --  ++ STANZA_PARAM  -> STRING
      --  ++                  TABLE
      procedure Read_Step (Step : in out Step_Type) is
         use Argument_Vectors;
         Continue    : Boolean := True;
         Long_String : Unbounded_String;
         Table       : Table_Type;
      begin
         Set_Stanza   (Step, Trimed_Suffix (Line_S, Idx_Data));
         Set_Position (Step, Position_Line, File_Name);
         while Continue and not Parser_End_Of_File loop

            Parser_Read_Line;

            if Detect_Keyword (K_Background)       or else
               Detect_Keyword (K_Scenario)         or else
               Detect_Keyword (K_Scenario_Outline) or else
               Detect_Keyword (K_Examples)         or else
               Detect_Keyword (K_Given)            or else
               Detect_Keyword (K_When)             or else
               Detect_Keyword (K_Then)             or else
               Detect_Keyword (K_And)              or else
               Detect_Keyword ("@")
            then
               Unread_Line := True;
               Continue    := False;
            elsif Detect_Keyword (K_StrSimple) then
               Read_String (Long_String, K_StrSimple);
               Append (Step.Args, Argument_Type'(Text, Long_String));
            elsif Detect_Keyword (K_StrDouble) then
               Read_String (Long_String, K_StrDouble);
               Append (Step.Args, Argument_Type'(Text, Long_String));
            elsif Detect_Keyword ("|") then
               Read_Table;
               Append (Step.Args, Argument_Type'(Table, Table));
            elsif Detect_Keyword ("#") then
               null;
            elsif Idx_Data > 0 then
               Log_Error    ("ERROR: invalid format");
               raise Parse_Error;
            end if;

         end loop;
--          Log_Error ("End   read step");
      end Read_Step;

      --  ++ STRING        -> """ NL { char | \" } NL { space } """ NL
      --  ++                | ''' NL { char | \' } NL { space } ''' NL
      procedure Read_String   (Result   : out    Unbounded_String;
                               Sep      : in     String)
      is
         Indent   : constant Integer := Integer'Max (Idx_Start, 1);
         Continue : Boolean := True;
         Res      : Unbounded_String;
         I        : Natural;
      begin
         Result := Null_Unbounded_String;
         if Trimed_Suffix (Line_S, Idx_Data) /= "" then
            Log_Error    ("ERROR: stray characers after " & Sep);
         end if;
         while Continue and not Parser_End_Of_File loop

            Parser_Read_Line;

            if Detect_Keyword (Sep) and then
               Trimed_Suffix (Line_S, Idx_Data) = ""
            then
               Head (Res, Length (Res) - 1);
               Continue := False;

            else
               declare
                  Line : constant String := To_String (Line_S);
               begin
                  I := Indent;
                  if Idx_Start /= 0 then
                     I := Natural'Min (Idx_Start, I);
                  end if;
                  while I <= Line'Last loop
                     if I <= Line'Last - 1 and then
                        Line (I .. I + 1) = "\" & Sep (Sep'First)
                     then
                        Append (Res, Sep (Sep'First));
                        I := I + 2;
                     else
                        Append (Res, Line (I));
                        I := I + 1;
                     end if;
                  end loop;
                  Append (Res, ASCII.LF);
               end;
            end if;

         end loop;
         Result := Res;
      end Read_String;

      --  ++ TABLE         -> { "|" { cell "|" } NL }
      procedure Read_Table is
         use Ada.Strings.Fixed;
         use Ada.Strings;
         use AdaSpecLib.String_Tables;
         X        : Natural := 0;
         Y        : Natural := 0;
         Table    : AdaSpecLib.String_Tables.Table;
         Continue : Boolean := True;
      begin
         while Continue and not Parser_End_Of_File loop
            if Y > 0 then
               Parser_Read_Line;
            end if;
            if Detect_Keyword ("|") then
               declare
                  Line : constant String :=
                         To_String (Trimed_Suffix (Line_S, Idx_Data));
                  I, N : Integer;
               begin
                  I := Line'First;
                  N := Index (Line, "|", I);
                  while N > 0 loop
                     Table.Put (X, Y,
                        To_Unbounded_String (Trim (Line (I .. N - 1), Both)));
--                      Log_Line ("Table (" & X'Img & Y'Img & " ) " &
--                                Trim (Line (I .. N - 1), Both));
                     I := N + 1;
                     N := Index (Line, "|", I);
                     X := X + 1;
                  end loop;
               end;
               Y := Y + 1;
               X := 0;
            else
               Unread_Line := True;
               Continue    := False;
            end if;
         end loop;
         Result := Table;
      end Read_Table;

   begin
      Read_All;
   end Parse;

end AdaSpec_Feature_Parser;
