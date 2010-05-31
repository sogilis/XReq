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

package body Sample2 is

   procedure I_am_in_front_of_a_cake_machine (Args : in out Arg_Type) is
      pragma Unreferenced (Args);
   begin
      Put_Line ("I am in front of a cake machine");
   end I_am_in_front_of_a_cake_machine;

   procedure I_insert_money (Args : in out Arg_Type) is
      pragma Unreferenced (Args);
   begin
      Put_Line ("I insert money");
   end I_insert_money;

   procedure I_push_the_button (Args : in out Arg_Type) is
      pragma Unreferenced (Args);
   begin
      Put_Line ("I push the button");
   end I_push_the_button;

   procedure I_get_a_cake (Args : in out Arg_Type) is
      pragma Unreferenced (Args);
   begin
      Put_Line ("I get a cake");
   end I_get_a_cake;

   procedure When_I_Match (Args : in out Arg_Type) is
   begin
      Put_Line ("When I match """ & Args.Match (1) & """");
   end When_I_Match;

end Sample2;
