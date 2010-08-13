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

with Ada.Directories;
with Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Fixed;
with GNAT.Regpat;
with Util.Strings;
with XReq.Step_Definitions.Handles;

use Ada.Directories;
use Ada.Text_IO;
use Ada.Strings;
use Ada.Strings.Fixed;
use GNAT.Regpat;
use Util.Strings;
use XReq.Step_Definitions.Handles;

package body XReq.Step_definitions.C is

   -----------------------
   --  Parse_Directory  --
   -----------------------

   procedure Parse_Directory (Steps      : in out Step_File_List_Handle;
                              Logger     : in     Logger_Ptr;
                              Directory  : in     String;
                              Fill_Steps : in     Boolean := False)
   is
      Search  : Search_Type;
      Element : Directory_Entry_Type;
      Step    : C_Step_File_Ptr;
      Step_H  : Step_File_Handle;
   begin
      Start_Search (Search, Directory, "*.h",
                    (Ordinary_File => True, others => False));
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Element);
         Step := new C_Step_File_Type;
         Step.Make  (Compose (Directory, Simple_Name (Element)), Fill_Steps);
         Step.Parse (Logger);
         Step_H.Set (Step_File_Ptr (Step));
         Steps.R.Append (Step_H);
      end loop;
      End_Search (Search);
   end Parse_Directory;

   ------------
   --  Make  --
   ------------

   procedure Make (S          : out C_Step_File_Type;
                   File_Name  : in  String;
                   Fill_Steps : in  Boolean := False) is
   begin
      S := (Reffy.Counted_Type (S) with
            File_Name  => To_Unbounded_String (File_Name),
            Parsed     => False,
            Fill_Steps => Fill_Steps,
            others     => <>);
   end Make;

   -------------
   --  Parse  --
   -------------

   procedure Parse     (S          : in out C_Step_File_Type;
                        Logger     : in     Logger_Ptr)
   is
      type Found_Type is (Found_None, Found_TODO, Found_Step,
                          Found_Regexp, Found_Regexp2);
      use Step_Container;
      Tokens        : constant String_List
                    := (To_Unbounded_String ("XREQ_GIVEN"),
                        To_Unbounded_String ("XREQ_WHEN"),
                        To_Unbounded_String ("XREQ_THEN"),
                        To_Unbounded_String ("@given"),
                        To_Unbounded_String ("@when"),
                        To_Unbounded_String ("@then"),
                        To_Unbounded_String ("XREQ_STEP_TODO"),
                        To_Unbounded_String ("XREQ_STEP"));
      File          : File_Type;
      Position      : Position_Type;
      Line_S        : Unbounded_String;
      Idx_Next      : Natural;
      Idx_Tk        : Natural;
      Idx, Idx2     : Natural;
      Prefix        : Step_Kind;
      Found         : Found_Type;
      Pattern       : Unbounded_String;
      Current_Steps : Step_Container.Vector;
      I             : Step_Container.Cursor;
      Current_Step  : Step_Definition_Type;
      Procedure_S   : Unbounded_String;
   begin
      Position.File := S.File_Name;
      Open (File, In_File, To_String (S.File_Name));
      while not End_Of_File (File) loop
         --
         --  Read Line
         --
         Line_S := Get_Whole_Line (File);
         Position.Line := Position.Line + 1;

         --
         --  Read Token
         --
         Find_Token (To_String (Line_S), Tokens, Idx_Next, Idx_Tk);
         case Idx_Tk is
            when 1 =>      Prefix := Step_Given; Found := Found_Regexp;
            when 2 =>      Prefix := Step_When;  Found := Found_Regexp;
            when 3 =>      Prefix := Step_Then;  Found := Found_Regexp;
            when 4 =>      Prefix := Step_Given; Found := Found_Regexp2;
            when 5 =>      Prefix := Step_When;  Found := Found_Regexp2;
            when 6 =>      Prefix := Step_Then;  Found := Found_Regexp2;
            when 7 =>      Found := Found_TODO;
            when 8 =>      Found := Found_Step;
            when others => Found := Found_None;
         end case;

         --
         --  XREQ_GIVEN, XREQ_WHEN, XREQ_THEN
         --
         if Found = Found_Regexp then
            Idx  := Index (Line_S, "(""", Idx_Next);
            Idx2 := Index (Line_S, """)", Backward);
            if Idx /= 0  and Idx2 /= 0 then
               Pattern := To_Unbounded_String
                 (Decode_Python (Slice (Line_S, Idx + 2, Idx2 - 1)));
               Current_Step := Step_Definition_Type'
                 (Ada.Finalization.Controlled with
                  Prefix    => Prefix,
                  Pattern_R => new Pattern_Matcher'(
                               Compile (To_String (Pattern))),
                  Pattern_S => Pattern,
                  Position  => Position,
                  others    => <>);
               Append (Current_Steps, Current_Step);
            else
               Logger.Put_Line
                 ("WARNING: Syntax error in " & To_String (Position));
            end if;

         --
         --  @given, @when, @then
         --
         elsif Found = Found_Regexp2 then
            Idx := Index_Non_Blank (Line_S, Idx_Next);
            if Idx /= 0 then
               Pattern := Unbounded_Slice (Line_S, Idx, Length (Line_S));
               Current_Step := Step_Definition_Type'
                 (Ada.Finalization.Controlled with
                  Prefix    => Prefix,
                  Pattern_R => new Pattern_Matcher'(
                               Compile (To_String (Pattern))),
                  Pattern_S => Pattern,
                  Position  => Position,
                  others    => <>);
               Append (Current_Steps, Current_Step);
            else
               --  TODO: use better reporting method
               Logger.Put_Line (String'(
                  "WARNING: Expecting argument in " &
                  To_String (Position) & ":" &
                  Trim (Idx_Next'Img, Left)));
            end if;

         --
         --  XREQ_STEP
         --
         elsif Found = Found_Step then
            Idx  := Index (Line_S, "(", Idx_Tk);
            Idx2 := Index (Line_S, ")", Idx);
            if Idx /= 0  and Idx2 /= 0 then
               Procedure_S := Unbounded_Slice (Line_S, Idx + 1, Idx2 - 1);
               I := First (Current_Steps);
               while Has_Element (I) loop
                  Current_Step := Element (I);
                  Current_Step.Proc_Name := Procedure_S;
                  S.Steps.Append (Current_Step);
                  Next (I);
               end loop;
               Clear (Current_Steps);
            else
               Logger.Put_Line
                 ("WARNING: Syntax error in " & To_String (Position));
            end if;

         --
         --  XREQ_STEP_TODO
         --
         elsif Found = Found_TODO then
            I := First (Current_Steps);
            while Has_Element (I) loop
               Current_Step := Element (I);
               Current_Step.Proc_Name := Null_Unbounded_String;
               S.Steps.Append (Current_Step);
               Next (I);
            end loop;
            Clear (Current_Steps);
         end if;
      end loop;
      Close (File);
      S.Parsed := True;
   end Parse;


end XReq.Step_Definitions.C;
