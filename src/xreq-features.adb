-------------------------------------------------------------------------------
--  XReq  --  Behaviour Driven Developpement tool for compiled languages     --
--  Copyright (c) 2010, SOGILIS <http://sogilis.com>                         --
--                                                                           --
--  This program is free software: you can redistribute it and/or modify     --
--  it under the terms of the GNU Affero General Public License as           --
--  published by the Free Software Foundation, either version 3 of the       --
--  License, or (at your option) any later version.                          --
--                                                                           --
--  This program is distributed in the hope that it will be useful,          --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of           --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            --
--  GNU Affero General Public License for more details.                      --
--                                                                           --
--  You should have received a copy of the GNU Affero General Public License --
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.    --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Directories;
with Ada.Strings;
with Ada.Strings.Fixed;
with Util.Strings;
with XReqLib.String_Tables;
with XReq.Args;
with XReq.Steps;
with XReq.Language;

use Util.Strings;
use XReq.Args;
use XReq.Steps;

package body XReq.Features is

   ------------------------------------------
   --  Generic_Feature_Type  --  Language  --
   ------------------------------------------

   function  Language  (F : in  Generic_Feature_Type) return Language_Handle is
   begin
      return F.Lang;
   end Language;

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
      use Ada.Directories;
      use String_Vectors;

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
      File_Ext : constant String := Extension (To_String (Self.File_Name));
      K : XReq.Language.Language_Type;

      --  Context variables
      File          : File_Type;        --  Current opened file
      Position      : Position_Type;    --  Current position
      First_Line    : Boolean := True;  --  True on the first line;
      Line_S        : Unbounded_String; --  Current line
      Idx_Start     : Natural;          --  First non blank on current line
      Idx_Data      : Natural;          --  Index where data starts
      --                                --  (after the keyword generally)
      Unread_Line   : Boolean := False; --  Unread the line;
      Current_Tags  : XReqLib.String_Vector;

      -------------------------
      --  Utility Functions  --
      -------------------------

      procedure Log_Error (Error : in String)
      is
         use Ada.Strings;
         use Ada.Strings.Fixed;
      begin
         if Log.Verbosity < 0 then
            Log.Put_Line (-1, To_String (Position) & ": " & Error);
         else
            Log.Put_Line (Error & " in " & To_String (Position.File) &
                          " line" & Position.Line'Img);
         end if;
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
         if Detect_Keyword (K.Feature) then
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
         Scenario         : Scenario_Type;
      begin
         Feature.Set_Name (To_String (Trimed_Suffix (Line_S, Idx_Data)));
         Feature.Set_Position (Position);
         while not End_Of_File loop
            Read_Line;
            if Detect_Keyword (K.Background) then
               Read_Scenario (Scenario);
               Feature.Set_Background (Scenario);
               Beginning := False;
            elsif Detect_Keyword (K.Scenario) then
               Current_Scenario := Null_Scenario;
               Read_Scenario (Current_Scenario);
               Self.Scenario_Append (Current_Scenario);
               Beginning := False;
            elsif Detect_Keyword (K.Scenario_Outline) then
               Current_Scenario := Null_Scenario;
               Read_Scenario (Current_Scenario, True);
               Self.Scenario_Append (Current_Scenario);
               Beginning := False;
            elsif Detect_Keyword ("#") then
               null;
            elsif Detect_Keyword ("@") then
               Read_Keywords;
            elsif Beginning then
               Data := Trimed_Suffix (Line_S, Idx_Data);
               if Data /= Null_Unbounded_String or Had_Description then
                  Feature.Append_Description (To_String (Data));
                  Had_Description := True;
               end if;
            end if;
         end loop;
      end Read_Feature;

      --  ++ SCENARIO      -> K.SCENARIO name NL
      --  ++                  { STANZA }
      --  ++                  [ SCENARIO_EX ]
      --  ++ K.SCENARIO    -> "Background:"
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
         Table          : String_Tables.Table;
      begin
         Scenario.Make
           (Name     => To_String (Trimed_Suffix (Line_S, Idx_Data)),
            Position => Position,
            Outline  => Outline,
            Tags     => Current_Tags);
         Clear (Current_Tags);
         while Continue and not End_Of_File loop

            Read_Line;
            Detect := True;

            if Detect_Keyword (K.Background)       or else
               Detect_Keyword (K.Scenario)         or else
               Detect_Keyword (K.Scenario_Outline) or else
               Detect_Keyword ("@")
            then
               Unread_Line := True;
               Continue    := False;
               Detect      := False;
            elsif Detect_Keyword ("#") then
               null;
            elsif Outline and then Detect_Keyword (K.Examples) then
               Read_Line;
               if not End_Of_File then
                  Read_Table (Table);
                  Scenario.Set_Table (Table);
               end if;
               Detect := False;
            elsif Detect_Keyword (K.Given) then
               Current_Prefix := Step_Given;
            elsif Detect_Keyword (K.When_K) then
               Current_Prefix := Step_When;
            elsif Detect_Keyword (K.Then_K) then
               Current_Prefix := Step_Then;
            elsif Detect_Keyword (K.And_K) then
               if Current_Prefix = Step_Null then
                  if Log.Verbosity < 0 then
                     Log_Error    ("ERROR: And keyword should be following " &
                                   "another keyword");
                     Log_Error    ("ERROR: Ignoring step");
                  else
                     Log_Error    ("ERROR: And keyword");
                     Log.Put_Line ("       And keyword should be following " &
                                          "another keyword");
                     Log.Put_Line ("       Ignoring step");
                  end if;
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
                  Scenario.Step_Append (Current_Stanza);
               end if;
            end if;

         end loop;
      end Read_Scenario;

      --  ++ STANZA        -> K.STANZA text NL
      --  ++                  { STANZA_PARAM }
      --  ++ K.STANZA      -> "Given"
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
         Step.Make (Step.Kind,
                    To_String (Trimed_Suffix (Line_S, Idx_Data)),
                    Position);
--          Log_Error ("Begin read step");
         while Continue and not End_Of_File loop

            Read_Line;

            if Detect_Keyword (K.Background)       or else
               Detect_Keyword (K.Scenario)         or else
               Detect_Keyword (K.Scenario_Outline) or else
               Detect_Keyword (K.Examples)         or else
               Detect_Keyword (K.Given)            or else
               Detect_Keyword (K.When_K)           or else
               Detect_Keyword (K.Then_K)           or else
               Detect_Keyword (K.And_K)            or else
               Detect_Keyword ("@")
            then
               Unread_Line := True;
               Continue    := False;
            elsif Detect_Keyword (K.StrSimple) then
               Read_String (Long_String, K.StrSimple);
               Step.Arg_Append (Argument_Type'(Text, Long_String));
            elsif Detect_Keyword (K.StrDouble) then
               Read_String (Long_String, K.StrDouble);
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
      procedure Read_Table (Result : out XReqLib.String_Tables.Table) is
         use Ada.Strings.Fixed;
         use Ada.Strings;
         use XReqLib.String_Tables;
         X        : Natural := 0;
         Y        : Natural := 0;
         Table    : XReqLib.String_Tables.Table;
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
      K.Set_Type (File_Ext);
      Self.Set_Filetype (File_Ext);
      Self.Lang.Set (new XReq.Language.Language_Type'(K));
      Position := Position_Type'(
         File => Self.File_Name,
         Line => 1);
      Open (File, In_File, To_String (Self.File_Name));
      Read_All;
      Close (File);
      Self.Parsed := True;
   exception
      when XReq.Language.Unknown_Type =>
         Log.Put_Line ("Unknown file format: " & File_Ext);
         raise Parse_Error;
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
      CRLF : constant String := "" & ASCII.LF;
   begin
      return "# File: " & To_String (F.File_Name) & CRLF &
             Feature_Type (F).To_String;
   end To_String;

   -------------------------------------
   --  Feature_File_Type  --  Parsed  --
   -------------------------------------

   function Parsed (F : in Feature_File_Type) return Boolean is
   begin
      return F.Parsed;
   end Parsed;


end XReq.Features;
