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
                        Stanza  : in  Step_Type) return Boolean
   is
      This : constant access constant Step_File_Type'Class := S'Access;
   begin
      return This.Find (Stanza) /= "";
   end Contains;

   ------------
   --  Find  --
   ------------

   function  Find      (S       : in  Step_File_Type;
                        Stanza  : in  Step_Type) return String
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
                        Stanza  : in  Step_Type;
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
                        Stanza  : in     Step_Type)
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
         if Step.Prefix = Stanza.Kind then
            declare
               Matched  : Match_Array (0 .. Paren_Count (Step.Pattern_R.all));
            begin
--                Put_Line ("XReq.Steps.Ada.Find: Match """ &
--                          Stanza.To_String & """ against |" &
--                          To_String (Step.Pattern_S) & "|");
               Match (Step.Pattern_R.all, Stanza.Stanza, Matched);
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

   ----------------
   --  Finalize  --
   ----------------

   procedure Finalize  (S       : in out Step_File_Type) is
   begin
      Finalize (S.Steps);
   end Finalize;

   ----------------
   --  Finalize  --
   ----------------

   procedure Finalize (Steps : in out Step_Container.Vector) is
      use Step_Container;
      I : Step_Container.Cursor := First (Steps);
      E : Step_Definition_Type;
   begin
      while Has_Element (I) loop
         E := Element (I);
         Free (E.Pattern_R);
         Next (I);
      end loop;
      Clear (Steps);
   end Finalize;

   ------------
   --  Free  --
   ------------

   procedure Free      (S : in out Step_File_Ptr) is
      procedure Dealloc is new Ada.Unchecked_Deallocation
         (Step_File_Type'Class, Step_File_Ptr);
   begin
      Dealloc (S);
   end Free;


   -----------------
   --  File_Name  --
   -----------------

   function  File_Name (S : in Step_File_Type) return String is
   begin
      return To_String (S.File_Name);
   end File_Name;

end XReq.Step_Definitions;
