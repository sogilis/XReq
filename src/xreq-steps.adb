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

with Ada.Strings.Fixed;

package body XReq.Steps is

   -------------------------------
   --  Step_Type  --  New_Step  --
   -------------------------------

   procedure Make     (Step     : in out Step_Type;
                       Kind     : in  Step_Kind;
                       Stanza   : in  String;
                       Position : in  Position_Type) is
   begin
      Step := Step_Type'(Reffy.Counted_Type (Step) with
                         Prefix   => Kind,
                         M_Stanza => To_Unbounded_String (Stanza),
                         Pos      => Position,
                         others   => <>);
   end Make;

   ----------------------------------
   --  Stanza_Type  --  To_String  --
   ----------------------------------

   function To_String (S : in Step_Type;
                       K : in Step_All_Kind := Step_Null) return String is
      Buffer : Unbounded_String;
   begin
      if S.Prefix = K then
         Append (Buffer, "And ");
      else
         case S.Prefix is
            when Step_Given => Append (Buffer, "Given ");
            when Step_When  => Append (Buffer, "When ");
            when Step_Then  => Append (Buffer, "Then ");
         end case;
      end if;
      Append (Buffer, S.M_Stanza);
      return To_String (Buffer);
   end To_String;

   ----------------------------------
   --  Stanza_Type  --  To_Regexp  --
   ----------------------------------

   function To_Regexp (S : in Step_Type) return String is
      use Ada.Strings.Fixed;
      Buffer    : Unbounded_String;
      Stanza    : constant String := To_String (S.M_Stanza);
      I         : Natural := Stanza'First;
      N         : Natural;
   begin
      case S.Prefix is
         when Step_Given => Append (Buffer, "@given ");
         when Step_When  => Append (Buffer, "@when ");
         when Step_Then  => Append (Buffer, "@then ");
      end case;
      Append (Buffer, "^");
      while I <= Stanza'Last loop
         case Stanza (I) is
            when '\' | '(' | ')' | '[' | ']' | '.' | '*' | '+' | '?' | '^' =>
               Append (Buffer, "\" & Stanza (I));
            when '"' =>
               N := Index (Stanza, """", I + 1);
               if N = 0 then
                  Append (Buffer, Stanza (I));
               else
                  Append (Buffer, """([^""]*)""");
                  I := N;
               end if;
            when others =>
               Append (Buffer, Stanza (I));
         end case;
         I := I + 1;
      end loop;
      Append (Buffer, "$");
      return To_String (Buffer);
   end To_Regexp;

   ----------------------------------
   --  Stanza_Type  --  Arg_First  --
   ----------------------------------

   function Arg_First (S : in Step_Type) return Natural is
      pragma Unreferenced (S);
   begin
      return 0;
   end Arg_First;

   ---------------------------------
   --  Stanza_Type  --  Arg_Last  --
   ---------------------------------

   function Arg_Last  (S : in Step_Type) return Integer is
      use Argument_Vectors;
      use Ada.Containers;
   begin
      return Integer (Length (S.Args)) - 1;
   end Arg_Last;

   ---------------------------------
   --  Stanza_Type  --  Arg_Last  --
   ---------------------------------

   function Arg_Element (S : in Step_Type;
                         I : in Natural)   return Argument_Type is
      use Argument_Vectors;
   begin
      return Element (S.Args, I);
   end Arg_Element;

   ---------------------------------
   --  Stanza_Type  --  Position  --
   ---------------------------------

   function Position (S : in Step_Type) return Position_Type is
   begin
      return S.Pos;
   end Position;

   -------------------------------
   --  Stanza_Type  --  Stanza  --
   -------------------------------

   function Stanza    (S : in Step_Type) return String is
   begin
      return To_String (S.M_Stanza);
   end Stanza;

   -----------------------------
   --  Stanza_Type  --  Kind  --
   -----------------------------

   function Kind      (S : in Step_Type) return Step_Kind is
   begin
      return S.Prefix;
   end Kind;
   -------------------------------------
   --  Stanza_Type  --  Set_Position  --
   -------------------------------------

   procedure Set_Position (S      : in out Step_Type;
                           Pos    : in     Position_Type) is
   begin
      S.Pos := Pos;
   end Set_Position;

   -----------------------------------
   --  Stanza_Type  --  Set_Stanza  --
   -----------------------------------

   procedure Set_Stanza   (S      : in out Step_Type;
                           Stanza : in     String) is
   begin
      S.M_Stanza := To_Unbounded_String (Stanza);
   end Set_Stanza;

   ---------------------------------
   --  Stanza_Type  --  Set_Kind  --
   ---------------------------------

   procedure Set_Kind     (S      : in out Step_Type;
                           Kind   : in     Step_Kind) is
   begin
      S.Prefix := Kind;
   end Set_Kind;

   -----------------------------------
   --  Stanza_Type  --  Arg_Append  --
   -----------------------------------

   procedure Arg_Append   (S      : in out Step_Type;
                           E      : in     Argument_Type) is
      use Argument_Vectors;
   begin
      Append (S.Args, E);
   end Arg_Append;

   -----------------------------
   --  Step_Type  --  Equals  --
   -----------------------------

   function Equals (Left, Right : in Step_Type) return Boolean is
      L : constant access constant Step_Type'Class := Left'Access;
      R : constant access constant Step_Type'Class := Right'Access;
   begin
      return L.all = R.all;
   end Equals;

end XReq.Steps;
