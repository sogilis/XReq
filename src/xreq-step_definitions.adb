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

   ----------------
   --  Contains  --
   ----------------

   function  Contains  (S       : in  Step_File_Type;
                        Stanza  : in  Step_Handle) return Boolean
   is
      This : constant access constant Step_File_Type'Class := S'Access;
   begin
      return This.Find (Stanza) /= "";
   end Contains;

   ------------
   --  Find  --
   ------------

   function  Find      (S       : in  Step_File_Type;
                        Stanza  : in  Step_Handle) return String
   is
      This    : constant access constant Step_File_Type'Class := S'Access;
      Proc    : Unbounded_String;
      Matches : Match_Vectors.Vector;
      Found   : Boolean;
   begin
      This.Find (Stanza, Proc, Matches, Found);
      if Found then
         return To_String (Proc);
      else
         return "";
      end if;
   end Find;

   ------------
   --  Find  --
   ------------

   procedure Find      (S       : in  Step_File_Type;
                        Stanza  : in  Step_Handle;
                        Proc    : out Unbounded_String;
                        Matches : out Match_Vectors.Vector;
                        Found   : out Boolean)
   is
      This   : constant access constant Step_File_Type'Class := S'Access;
      Result : constant Step_Match_Type := This.Find (Stanza);
   begin
      Found   := Result.Match;
      Proc    := Result.Proc_Name;
      Matches := Result.Matches;
   end Find;

   ------------
   --  Find  --
   ------------

   function  Find      (S       : in     Step_File_Type;
                        Stanza  : in     Step_Handle)
                                  return Step_Match_Type
   is
      use Match_Vectors;
      Result   : Step_Match_Type;
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
--                Put_Line ("XReq.Steps.Ada.Find: Match """ &
--                          Stanza.To_String & """ against |" &
--                          To_String (Step.Pattern_S) & "|");
               Match (Step.Pattern_R.all, Stanza.R.Stanza, Matched);
               if Matched (0) /= No_Match then
                  if Result.Match then
                     raise Ambiguous_Match;
                  end if;
--                   Put_Line ("Matched (0) = " & Stanza.Stanza
--                               (Matched (0).First .. Matched (0).Last));
                  Result.Proc_Name := Step.Proc_Name;
                  Result.Position  := Step.Position;
                  Result.Match     := True;
                  for J in Matched'First + 1 .. Matched'Last loop
--                      Put_Line ("Matched (" & J'Img & ") = " &
--                               Slice (Stanza.Stanza,
--                                     Matched (J).First,
--                                     Matched (J).Last));
                     if Matched (J) /= No_Match then
                        Append (Matches2, Match_Location'(Matched (J).First,
                                                          Matched (J).Last));
                     end if;
                  end loop;  --  GCOV_IGNORE
                  Result.Matches := Matches2;
--                else
--                   Put_Line ("Matched (0) = No_Match");
               end if;
            end;  --  GCOV_IGNORE
--          else
--             Put_Line ("XReq.Steps.Ada.Find: Don't Match " &
--                       Stanza.Prefix'Img & " """ &
--                       To_String (Stanza.Stanza) & """ against " &
--                       Step.Prefix'Img & " |" &
--                       To_String (Step.Pattern_S) & "|");
         end if;
      end loop;

      --  No match
--       Put_Line ("XReq.Steps.Ada.Find: Not found");
      return Result;
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
