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

package XReqLib.Interface_Scenarios is

   type Scenario_Interface is interface;

--  procedure Step_Append  (Scenario : in out Scenario_Interface;
--                         Stanza   : in     Step_Type)            is abstract;
   function  Step_First   (Scenario : in     Scenario_Interface)
                                      return Natural               is abstract;
   function  Step_Last    (Scenario : in     Scenario_Interface)
                                      return Integer               is abstract;
   function  Step_Count   (Scenario : in     Scenario_Interface)
                                      return Natural               is abstract;
--  function  Step_Element (Scenario : in     Scenario_Interface;
--                         Index    : in     Natural)
--                                    return Step_Type             is abstract;

   function  Outline      (S : in Scenario_Interface) return Boolean
                                                                   is abstract;
   function  Name         (S : in Scenario_Interface) return String
                                                                   is abstract;
   function  Position     (S : in Scenario_Interface) return Position_Type
                                                                   is abstract;
   function  Tag_Vector   (S : in Scenario_Interface) return String_Vector
                                                                   is abstract;
   function  Table        (S : in Scenario_Interface)
                           return XReqLib.String_Tables.Table   is abstract;

   procedure Set_Table    (S     : in out Scenario_Interface;
                           Table : in     XReqLib.String_Tables.Table)
                                                                   is abstract;

   procedure Output_Steps (S     : in     Scenario_Interface;
                           Buf   : in out Unbounded_String)        is abstract;

end XReqLib.Interface_Scenarios;
