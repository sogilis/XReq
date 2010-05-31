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

with Ada.Strings.Unbounded;
with XReqLib.String_Tables;

use Ada.Strings.Unbounded;

package XReq.Args is

   ---------------------
   --  Argument_Type  --
   ---------------------

   type Argument_Kind is (None, Text, Table);

   type Argument_Type (Typ : Argument_Kind := None) is
      record
         --  GCOV_IGNORE_BEGIN
         case Typ is
            when Text =>
               Text : Unbounded_String;
            when Table =>
               Table : XReqLib.String_Tables.Table;
            when None =>
               null;
         end case;
         --  GCOV_IGNORE_END
      end record;

   function  Text     (A : in     Argument_Type) return String;
   procedure Set_Text (A : in out Argument_Type;
                       T : in     String);

end XReq.Args;
