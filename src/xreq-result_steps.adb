--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with XReqLib;

use Ada.Strings.Unbounded;
use XReqLib;

package body XReq.Result_Steps is


   ----------------------------------
   --  Result_Step_Type  --  Make  --
   ----------------------------------

   procedure Make           (Self           : out Result_Step_Type;
                             Step           : in  Step_Type;
                             Match          : in  Step_Match_Type
                                            := Step_Match_Type'(others => <>))
   is
   begin
      Self := Result_Step_Type'(Step with Match => Match);
   end Make;

   ---------------------------------------------
   --  Result_Step_Type  --  New_Result_Step  --
   ---------------------------------------------

   function  New_Result_Step (Step           : in  Step_Type;
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
      E : Match_Location;
   begin
      Append (Buffer, Indent & Procedure_Name (S) & " (");
      for I in S.Match_First .. S.Match_Last loop
         E := S.Match_Element (I);
         Append (Buffer, String'("(" &
                 Ada_String (S.Stanza (E.First .. E.Last)) &
                 E.First'Img &
                 E.Last'Img & ")"));
      end loop;
      Append (Buffer, " );");
      return To_String (Buffer);
   end To_Code;

   ------------------------------------------
   --  Result_Step_Type  --  Process_Step  --
   ------------------------------------------

   procedure Process_Step     (Res           : out    Result_Step_Type;
                               Stanza        : in     Step_Type;
                               Steps         : in     Step_Definitions_Type;
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
         Match := Find (Steps, Stanza);
      exception
         when Ambiguous_Match =>
            if Log.Verbosity < 0 then
               Log.Put_Line (-1, To_String (Stanza.Position) & ": ERROR: " &
                             "Ambiguous match for: " & Stanza.To_String);
            else
               Log.Put_Line ("ERROR: Ambiguous match in " &
                             To_String (Stanza.Position) & " for:");
               Log.Put_Line ("  " & Stanza.To_String);
            end if;
            Errors := True;
      end;
      if not Match.Match then
         RegExp := To_Unbounded_String (Stanza.To_Regexp);
         Include (Missing_Steps, RegExp);
         if Log.Verbosity < 0 then
            Log.Put_Line (-1, To_String (Stanza.Position) & ": ERROR: " &
                          "Missing step definition for: " & Stanza.To_String);
         else
            Log.Put_Line (String'("ERROR: Missing step definition in " &
                          To_String (Stanza.Position) & " for:"));
            Log.Put_Line ("  " & Stanza.To_String);
            Log.Put_Line ("You can implement this step by adding on your " &
                          "step definition file:");
            Log.Put_Line ("  --  " & Stanza.To_Regexp);
            Log.Put_Line ("  --  @todo");
            Log.New_Line;
         end if;
         Errors := True;
      elsif Step_Matching then
         Log.Put_Line ("Step Matching: """ & To_String (Stanza.Position) &
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
      use Match_Vectors;
   begin
      return Integer (Length (S.Match.Matches));
   end Match_Count;

   ------------------------------------
   --  Step_Type  --  Match_Element  --
   ------------------------------------

   function Match_Element (S : in Result_Step_Type;
                           I : in Natural)          return Match_Location
   is
      use Match_Vectors;
   begin
      return Element (S.Match.Matches, I);
   end Match_Element;

end XReq.Result_Steps;
