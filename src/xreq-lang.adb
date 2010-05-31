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
with Ada.Strings.Maps.Constants;

use Ada.Strings.Fixed;
use Ada.Strings.Maps.Constants;

package body XReq.Lang is

   function Get_Language (Lang : in String) return Language_Type
   is
      L : constant String := Translate (Lang, Lower_Case_Map);
   begin
      if L = "ada" then
         return Lang_Ada;
      elsif L = "c" then
         return Lang_C;
      else
         raise Invalid_Language with "Unknown language " & Lang;
      end if;
   end Get_Language;

end XReq.Lang;
