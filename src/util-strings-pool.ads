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

with Ada.Containers.Hashed_Sets;
with Ada.Strings.Unbounded.Hash;

package Util.Strings.Pool is

   -------------------
   --  String_Pool  --
   -------------------

   type String_Pool is private;

   Empty_Pool : constant String_Pool;

   procedure Add_Pool          (Pool   : in out String_Pool;
                                Str    : in     String);
   procedure Get_Unique_String (Pool   : in out String_Pool;
                                Base   : in     String;
                                Result : out    Unbounded_String);

private

   package String_Pools is
      new Ada.Containers.Hashed_Sets (Unbounded_String, Hash, "=", "=");

   type String_Pool is
      record
         Set : String_Pools.Set;
      end record;

   Empty_Pool : constant String_Pool := (others => <>);

end Util.Strings.Pool;
