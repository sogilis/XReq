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

with GNAT.Regpat;
with Ada.Unchecked_Deallocation;

use GNAT.Regpat;

package body XReq.Step_Definitions is

   --------------
   --  Parsed  --
   --------------

   function  Parsed    (S       : in  Step_File_Type) return Boolean is
   begin
      return S.Parsed;
   end Parsed;

   ------------
   --  Find  --
   ------------

   procedure Find      (S       : in     Step_File_Type;
                        Stanza  : in     Step_Handle;
                        Log     : in     Logger_Ptr;
                        Result  : in     Find_Result_Procedure)
   is
      use Match_Vectors;
      Res   : Step_Match_Type;
      Step     : Step_Definition_Type;
      Matches2 : Match_Vectors.Vector;
   begin

      --  Error if not parsed
      if not S.Parsed then
         raise Unparsed_Step;
      end if;

      --  Look for the phrase
      for i in S.Steps.First_Index .. S.Steps.Last_Index loop
         Step  := S.Steps.Element (i);
         if Step.Prefix = Stanza.R.Kind then
            declare
               Matched  : Match_Array (0 .. Paren_Count (Step.Pattern_R.all));
            begin
               Log.Put_Line (2, "XReq.Steps.Ada.Find: Match """ &
                           Stanza.Ref.To_String & """ against |" &
                           To_String (Step.Pattern_S) & "|");
               Match (Step.Pattern_R.all, Stanza.R.Stanza, Matched);
               if Matched (0) /= No_Match then
                  Log.Put_Line (2, String '("Matched (0) = " & Stanza.R.Stanza
                             (Matched (0).First .. Matched (0).Last)));
                  Res.Proc_Name := Step.Proc_Name;
                  Res.Position  := Step.Position;
                  Res.Match     := True;
                  for J in Matched'First + 1 .. Matched'Last loop
                     if Matched (J) /= No_Match then
                        Log.Put_Line
                          (2, "Matched (" & J'Img & ") = " &
                             Stanza.R.Stanza
                             (Matched (J).First .. Matched (J).Last));
                        Append (Matches2, Match_Location'(Matched (J).First,
                                                          Matched (J).Last));
                     end if;
                  end loop;  --  GCOV_IGNORE
                  Res.Matches := Matches2;
                  Result (Res);
               else
                  Log.Put_Line (2, "Matched (0) = No_Match");
               end if;
            end;  --  GCOV_IGNORE
         else
            Log.Put_Line (2, String'("XReq.Steps.Ada.Find: Don't Match " &
                            Stanza.R.Kind'Img & " """ &
                            Stanza.R.Stanza & """ against " &
                            Step.Prefix'Img & " |" &
                            To_String (Step.Pattern_S) & "|"));
         end if;
      end loop;

      --  No match
      Log.Put_Line (2, "XReq.Steps.Ada.Find: Not found");
   end Find;

   -----------------
   --  File_Name  --
   -----------------

   function  File_Name (S : in Step_File_Type) return String is
   begin
      return To_String (S.File_Name);
   end File_Name;

   ----------------
   --  Finalize  --
   ----------------

   procedure Finalize  (S       : in out Step_File_Type) is
      use Step_Container;
   begin
      Clear (S.Steps);
   end Finalize;

   ----------------------------------------------------------------------------

   procedure Initialize (Object : in out Step_Definition_Type) is
   begin
      null;
   end Initialize;

   procedure Adjust     (Object : in out Step_Definition_Type) is
   begin
      if Object.Pattern_R /= null then
         Object.Pattern_R := new Pattern_Matcher'(Object.Pattern_R.all);
      end if;
   end Adjust;

   procedure Finalize   (Object : in out Step_Definition_Type) is
      procedure Free is new Ada.Unchecked_Deallocation
        (GNAT.Regpat.Pattern_Matcher, Pattern_Matcher_Ptr);
   begin
      Free (Object.Pattern_R);
   end Finalize;

end XReq.Step_Definitions;
