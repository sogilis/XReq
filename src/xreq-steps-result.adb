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

with XReq.Step_Definitions;

package body XReq.Steps.Result is


   ----------------------------------
   --  Result_Step_Type  --  Make  --
   ----------------------------------

   procedure Make           (Self           : in out Result_Step_Type;
                             Step           : in  Step_Handle;
                             Match          : in  Step_Match_Type
                                            := Step_Match_Type'(others => <>))
   is
      --  Super : Step_Type'Class := Self;
      Other : constant Step_Type := Step_Type (Step.Ref.all);
   begin
      --  TODO: find a better way
      Self.Prefix   := Other.Prefix;
      Self.M_Stanza := Other.M_Stanza;
      Self.Pos      := Other.Position;
      Self.Args     := Other.Args;
      Self := Result_Step_Type'(Step_Type (Self) with
         Match => Match);
   end Make;

   ---------------------------------------------
   --  Result_Step_Type  --  New_Result_Step  --
   ---------------------------------------------

   function  New_Result_Step (Step           : in  Step_Handle;
                              Match          : in  Step_Match_Type
                                             := Step_Match_Type'(others => <>))
                                             return Result_Step_Type
   is
   begin
      return Self : Result_Step_Type do
         Self.Make (Step, Match);
      end return;
   end New_Result_Step;


   -------------------------------------
   --  Result_Step_Type  --  To_Code  --
   -------------------------------------

   function To_Code   (S      : in Result_Step_Type;
                       Indent : in String := "") return String
   is
      Buffer : Unbounded_String;
      E : Step_Match_Location;
      Stanza : constant String := S.Stanza;
   begin
      Append (Buffer, Indent & Procedure_Name (S) & " (");
      for I in S.Match_First .. S.Match_Last loop
         E := S.Match_Element (I);
         Append (Buffer, String'("(" &
                 Ada_String (Stanza (E.First .. E.Last)) &
                 E.First'Img &
                 E.Last'Img & ")"));
      end loop;
      Append (Buffer, " );");
      return To_String (Buffer);
   end To_Code;

   ------------------------------------------
   --  Result_Step_Type  --  Process_Step  --
   ------------------------------------------

   procedure Process_Step     (Res           : in out Result_Step_Type;
                               Stanza        : in     Step_Handle;
                               Steps         : in     Step_File_List_Handle;
                               Log           : in     Logger_Ptr;
                               Errors        : out    Boolean;
                               Step_Matching : in     Boolean;
                               Missing_Steps : in out String_Set)
   is
      use String_Sets;
      Match  : Step_Match_Type;
      RegExp : Unbounded_String;
   begin
      Errors := False;
      begin
         Match := Steps.Ref.all.Find (Stanza);
      exception
         when XReq.Step_Definitions.Ambiguous_Match =>
            if Log.Verbosity < 0 then
               Log.Put_Line (-1, To_String (Stanza.R.Position) & ": ERROR: " &
                             "Ambiguous match for: " & Stanza.R.To_String);
            else
               Log.Put_Line ("ERROR: Ambiguous match in " &
                             To_String (Stanza.R.Position) & " for:");
               Log.Put_Line ("  " & Stanza.R.To_String);
            end if;
            Errors := True;
      end;
      if not Match.Match then
         RegExp := To_Unbounded_String (Stanza.R.To_Regexp);
         Include (Missing_Steps, RegExp);
         if Log.Verbosity < 0 then
            Log.Put_Line (-1, To_String (Stanza.R.Position) & ": ERROR: " &
                         "Missing step definition for: " & Stanza.R.To_String);
         else
            Log.Put_Line (String'("ERROR: Missing step definition in " &
                          To_String (Stanza.R.Position) & " for:"));
            Log.Put_Line ("  " & Stanza.R.To_String);
            Log.Put_Line ("You can implement this step by adding on your " &
                          "step definition file:");
            Log.Put_Line ("  --  " & Stanza.R.To_Regexp);
            Log.Put_Line ("  --  @todo");
            Log.New_Line;
         end if;
         Errors := True;
      elsif Step_Matching then
         Log.Put_Line ("Step Matching: """ & To_String (Stanza.R.Position) &
                       """ matches """ & To_String (Match.Position) &
                       """ procedure " & To_String (Match.Proc_Name));
      end if;
      Res.Make (Stanza, Match);
   end Process_Step;

   ---------------------------------------
   --  Result_Step_Type  --  File_Name  --
   ---------------------------------------

   function File_Name (S : in Result_Step_Type) return String is
   begin
      return To_String (S.Match.Position.File);
   end File_Name;

   --------------------------------------------
   --  Result_Step_Type  --  Procedure_Name  --
   --------------------------------------------

   function Procedure_Name (S : in Result_Step_Type) return String is
   begin
      return To_String (S.Match.Proc_Name);
   end Procedure_Name;

   --------------------------
   --  Set_Procedure_Name  --
   --------------------------

   procedure Set_Procedure_Name (S   : in out Result_Step_Type;
                                 Prc : in     String) is
   begin
      S.Match.Proc_Name := To_Unbounded_String (Prc);
   end Set_Procedure_Name;

   ----------------------------------
   --  Step_Type  --  Match_First  --
   ----------------------------------

   function Match_First   (S : in Result_Step_Type) return Natural is
      pragma Unreferenced (S);
   begin
      return 0;
   end Match_First;

   ---------------------------------
   --  Step_Type  --  Match_Last  --
   ---------------------------------

   function Match_Last    (S : in Result_Step_Type) return Integer is
   begin
      return Match_Count (S) - 1;
   end Match_Last;

   ----------------------------------
   --  Step_Type  --  Match_Count  --
   ----------------------------------

   function Match_Count   (S : in Result_Step_Type) return Natural is
      use Step_Match_Vectors;
   begin
      return Integer (Length (S.Match.Matches));
   end Match_Count;

   ------------------------------------
   --  Step_Type  --  Match_Element  --
   ------------------------------------

   function Match_Element (S : in Result_Step_Type;
                           I : in Natural)          return Step_Match_Location
   is
      use Step_Match_Vectors;
   begin
      return Element (S.Match.Matches, I);
   end Match_Element;

end XReq.Steps.Result;
