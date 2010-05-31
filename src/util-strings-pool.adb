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

package body Util.Strings.Pool is

   ----------------
   --  Add_Pool  --
   ----------------

   procedure Add_Pool          (Pool   : in out String_Pool;
                                Str    : in     String)
   is
      S : constant Unbounded_String := To_Unbounded_String (Str);
   begin
      if not Pool.Set.Contains (S) then
         Pool.Set.Insert (S);
      end if;
   end Add_Pool;

   -------------------------
   --  Get_Unique_String  --
   -------------------------

   procedure Get_Unique_String (Pool   : in out String_Pool;
                                Base   : in     String;
                                Result : out    Unbounded_String)
   is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use String_Pools;
      I : Natural := 2;
      S : Unbounded_String;
   begin
      S := To_Unbounded_String (Base);
      while Pool.Set.Contains (S) loop
         S := To_Unbounded_String (Base) & "_" & Trim (I'Img, Side => Left);
         I := I + 1;
      end loop;
      Pool.Set.Insert (S);
      Result := S;
   end Get_Unique_String;

end Util.Strings.Pool;
