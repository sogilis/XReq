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

with Ada.Command_Line;
with GNAT.OS_Lib;
with XReqLib.Format;

use Ada.Command_Line;
use XReqLib.Format;

package XReqLib.CLI is

   subtype Argument_List_Access is GNAT.OS_Lib.Argument_List_Access;

   procedure Get_Arguments   (Args : out    Argument_List_Access);
   procedure Free            (Arg  : in out Argument_List_Access)
      renames GNAT.OS_Lib.Free;

   procedure Parse_Arguments (Args       : in out Argument_List_Access;
                              Format     : out    Format_Ptr;
                              Continue   : out    Boolean;
                              Cond       : out    Conditional_Type;
                              List_Mode  : out    Boolean;
                              Success    : out    Boolean;
                              Name       : in     String := Command_Name);

   procedure Parse_Arguments (Format     : out    Format_Ptr;
                              Continue   : out    Boolean;
                              Cond       : out    Conditional_Type;
                              List_Mode  : out    Boolean;
                              Name       : in     String := Command_Name);

   procedure Help (Name : in String := Command_Name);

end XReqLib.CLI;
