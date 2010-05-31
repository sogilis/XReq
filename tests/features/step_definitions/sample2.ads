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

with XReqLib.General;

use  XReqLib.General;

package Sample2 is

   --  @given ^I am in front of a cake machine$
   procedure I_am_in_front_of_a_cake_machine (Args : in out Arg_Type);

   --  @when ^I insert money$
   procedure I_insert_money (Args : in out Arg_Type);

   --  @when ^I push the button$
   procedure I_push_the_button (Args : in out Arg_Type);

   --  @then ^I get a cake$
   procedure I_get_a_cake (Args : in out Arg_Type);

   --  @when ^I match "([^"]*)"$
   --  @when ^I match "([^"]*)" and "([^"]*)"$
   procedure When_I_Match (Args : in out Arg_Type);

end Sample2;
