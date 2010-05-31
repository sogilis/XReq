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

with Text_IO;

use Text_IO;

package body Sample1 is

   --  @given (this step works)
   procedure This_Step_Works (Args : in out Arg_Type) is
      pragma Unreferenced (Args);
   begin
      Put_Line ("this step works");
   end This_Step_Works;

   --  @when (this step works too)
   procedure This_Step_Works_Too (Args : in out Arg_Type) is
      pragma Unreferenced (Args);
   begin
      Put_Line ("this step works too");
   end This_Step_Works_Too;

   --  @given

   --  @when toto

   --  @then tata

end Sample1;
