--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;
with AdaSpec.Args;

use AdaSpec.Args;

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
                     Stanza   : in     Step_Type)
   is
      use Step_Vectors;
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
      F : constant Feature_File_Type := (others => <>);
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
                    Log    : in     Logger_Ptr)
   is

      Self : constant access Feature_File_Type'Class := F'Access;

      use Ada.Text_IO;
      use Util.Strings.Vectors;
      use Scenario_Container;
      use Step_Vectors;

      procedure Log_Error (Error : in String);
      procedure Read_Line;
      function  End_Of_File return Boolean;
      function  Detect_Keyword (Keyword : in String) return Boolean;

      procedure Read_All;
      procedure Read_Keywords;
      procedure Read_Feature  (Feature  : in out Feature_File_Type);
      procedure Read_Scenario (Scenario : out    Scenario_Type;
                               Outline  : in     Boolean := False);
      procedure Read_Step     (Step     : in out Step_Type);
      procedure Read_String   (Result   : out    Unbounded_String;
                               Sep      : in     String);
      procedure Read_Table    (Result   : out    String_Tables.Table);

      --  Keywords
      K_Feature          : constant String := "Feature:";
      K_Background       : constant String := "Background:";
      K_Scenario         : constant String := "Scenario:";
      K_Scenario_Outline : constant String := "Scenario Outline:";
      K_Examples         : constant String := "Examples:";
      K_Given            : constant String := "Given ";
      K_When             : constant String := "When ";
      K_Then             : constant String := "Then ";
      K_And              : constant String := "And ";
      K_StrDouble        : constant String := """""""";
      K_StrSimple        : constant String := "'''";

      --  Context variables
      File          : File_Type;        --  Current opened file
      Position      : Position_Type;    --  Current position
      First_Line    : Boolean := True;  --  True on the first line;
      Line_S        : Unbounded_String; --  Current line
      Idx_Start     : Natural;          --  First non blank on current line
      Idx_Data      : Natural;          --  Index where data starts
      --                                --  (after the keyword generally)
      Unread_Line   : Boolean := False; --  Unread the line;
      Current_Tags  : String_Vector;

      -------------------------
      --  Utility Functions  --
      -------------------------

      procedure Log_Error (Error : in String)
      is
      begin
         Log.Put_Line (Error & " in " & To_String (Position.File) & " line" &
                       Position.Line'Img);
      end Log_Error;

      procedure Read_Line is
      begin
         if Unread_Line then
            Unread_Line := False;
         else
            if not First_Line then
               Position.Line := Position.Line + 1;
            end if;
            First_Line := False;
            Line_S     := Get_Whole_Line (File);
            Idx_Start  := Index_Non_Blank (Line_S);
            Idx_Data   := Idx_Start;
         end if;
      end Read_Line;

      function  End_Of_File return Boolean is
      begin
         return not Unread_Line and then End_Of_File (File);
      end End_Of_File;

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
         Read_Line;
         if Detect_Keyword (K_Feature) then
            Read_Feature (F);
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
      procedure Read_Feature (Feature : in out Feature_File_Type) is
         Current_Scenario : Scenario_Type;
         Beginning        : Boolean := True;
         Had_Description  : Boolean := False;
         Data             : Unbounded_String;
         I                : Util.Strings.Vectors.Cursor;
      begin
         Feature.Name := Trimed_Suffix (Line_S, Idx_Data);
         Feature.Pos  := Position;
         while not End_Of_File loop
            Read_Line;
            if Detect_Keyword (K_Background) then
               Read_Scenario (Feature.Background);
               Beginning := False;
            elsif Detect_Keyword (K_Scenario) then
               Current_Scenario := Null_Scenario;
               Read_Scenario (Current_Scenario);
               Append (Self.Scenarios, Current_Scenario);
               Beginning := False;
            elsif Detect_Keyword (K_Scenario_Outline) then
               Current_Scenario := Null_Scenario;
               Read_Scenario (Current_Scenario, True);
               Append (Self.Scenarios, Current_Scenario);
               Beginning := False;
            elsif Detect_Keyword ("#") then
               null;
            elsif Detect_Keyword ("@") then
               Read_Keywords;
            elsif Beginning then
               Data := Trimed_Suffix (Line_S, Idx_Data);
               if Data /= Null_Unbounded_String or Had_Description then
                  Append (Feature.Description, Data);
                  Had_Description := True;
               end if;
            end if;
         end loop;
         --  Clean up desription
         I := Last (Feature.Description);
         while Has_Element (I) and then
               Element (I) = Null_Unbounded_String
         loop
            Delete_Last (Feature.Description);
            I := Last (Feature.Description);
         end loop;
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
         Current_Stanza : Step_Type;
         Current_Prefix : Step_All_Kind := Step_Null;
         Detect         : Boolean;
         Continue       : Boolean := True;
      begin
         if Outline then
            Scenario := (Outline => True,
                         Name    => Trimed_Suffix (Line_S, Idx_Data),
                         Pos     => Position,
                         Tags    => Current_Tags,
                         others  => <>);
         else
            Scenario := (Outline => False,
                         Name    => Trimed_Suffix (Line_S, Idx_Data),
                         Pos     => Position,
                         Tags    => Current_Tags,
                         others  => <>);
         end if;
         Clear (Current_Tags);
         while Continue and not End_Of_File loop

            Read_Line;
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
               Read_Line;
               if not End_Of_File then
                  Read_Table (Scenario.Table);
               end if;
               Detect := False;
            elsif Detect_Keyword (K_Given) then
               Current_Prefix := Step_Given;
            elsif Detect_Keyword (K_When) then
               Current_Prefix := Step_When;
            elsif Detect_Keyword (K_Then) then
               Current_Prefix := Step_Then;
            elsif Detect_Keyword (K_And) then
               if Current_Prefix = Step_Null then
                  Log_Error    ("ERROR: And keyword");
                  Log.Put_Line ("       And keyword should be following " &
                                       "another keyword");
                  Log.Put_Line ("       Ignoring step");
               end if;
            elsif Trimed_Suffix (Line_S, Idx_Data) /= "" then
               Log_Error    ("ERROR: invalid format");
               raise Parse_Error;
            else
               Detect := False;
            end if;

            if Detect then
               if Current_Prefix /= Step_Null then
                  Current_Stanza.Set_Kind (Current_Prefix);
               end if;
               Read_Step (Current_Stanza);
               if Current_Prefix /= Step_Null then
                  Append (Scenario.Stanzas, Current_Stanza);
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
         Continue    : Boolean := True;
         Long_String : Unbounded_String;
         Tble        : String_Tables.Table;
      begin
         Step := New_Step (Step.Kind,
                           To_String (Trimed_Suffix (Line_S, Idx_Data)),
                           Position);
--          Log_Error ("Begin read step");
         while Continue and not End_Of_File loop

            Read_Line;

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
               Step.Arg_Append (Argument_Type'(Text, Long_String));
            elsif Detect_Keyword (K_StrDouble) then
               Read_String (Long_String, K_StrDouble);
               Step.Arg_Append (Argument_Type'(Text, Long_String));
            elsif Detect_Keyword ("|") then
               Read_Table (Tble);
               Step.Arg_Append (Argument_Type'(Table, Tble));
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
         while Continue and not End_Of_File loop

            Read_Line;

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
      procedure Read_Table (Result : out AdaSpecLib.String_Tables.Table) is
         use Ada.Strings.Fixed;
         use Ada.Strings;
         use AdaSpecLib.String_Tables;
         X        : Natural := 0;
         Y        : Natural := 0;
         Table    : AdaSpecLib.String_Tables.Table;
         Continue : Boolean := True;
      begin
         while Continue and not End_Of_File loop
            if Y > 0 then
               Read_Line;
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
--                      Log.Put_Line ("Table (" & X'Img & Y'Img & " ) " &
--                                    Trim (Line (I .. N - 1), Both));
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
      Position := Position_Type'(
         File => Self.File_Name,
         Line => 1);
      Open (File, In_File, To_String (Self.File_Name));
      Read_All;
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

      procedure Output_Stanzas (V : in Step_Vectors.Vector);
      procedure Output_Stanzas (V : in Step_Vectors.Vector) is
         use Step_Vectors;
         Pre  : Step_All_Kind := Step_Null;
         Cur  : Step_Vectors.Cursor := First (V);
         Sta  : Step_Type;
      begin
         while Has_Element (Cur) loop
            Append (Res, "    ");
            Sta := Element (Cur);
            if Sta.Kind = Pre then
               Append (Res, "And");
            else
               case Sta.Kind is
                  when Step_Given => Append (Res, "Given");
                  when Step_When =>  Append (Res, "When");
                  when Step_Then =>  Append (Res, "Then");
               end case;
               Pre := Sta.Kind;
            end if;
            Append (Res, " ");
            Append (Res, Sta.Stanza);
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
