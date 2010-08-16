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

with Reffy.Abstract_Handles;

generic

   type Object_Type (<>) is new Counted with private;
   type Object_Ptr is access all Object_Type'Class;

package Reffy.Handles is

   package Parent_Pkg is new Reffy.Abstract_Handles (Object_Type, Object_Ptr);

   type Handle is new Parent_Pkg.Handle with null record;

   procedure Set_New  (H : in out Handle; Obj : Object_Type);
--   procedure Set_New  (H : in out Handle);

end Reffy.Handles;

