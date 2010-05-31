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

package XReqLib.Interface_Steps is

   type Step_Interface is interface;

   function  To_String    (S : in Step_Interface;
                           K : in Step_All_Kind := Step_Null)
                                                  return String    is abstract;

   function  Arg_First    (S : in Step_Interface) return Natural   is abstract;
   function  Arg_Last     (S : in Step_Interface) return Integer   is abstract;
--  function  Arg_Element  (S : in Step_Interface;
--                         I : in Natural)        return Argument_Type
--                                                                 is abstract;

   function  Position     (S : in Step_Interface) return Position_Type
                                                                   is abstract;
   function  Stanza       (S : in Step_Interface) return String    is abstract;
   function  Kind         (S : in Step_Interface) return Step_Kind is abstract;

   procedure Set_Position (S      : in out Step_Interface;
                           Pos    : in     Position_Type)          is abstract;
   procedure Set_Stanza   (S      : in out Step_Interface;
                           Stanza : in     String)                 is abstract;
   procedure Set_Kind     (S      : in out Step_Interface;
                           Kind   : in     Step_Kind)              is abstract;
--  procedure Arg_Append   (S      : in out Step_Interface;
--                         E      : in     Argument_Type)          is abstract;

end XReqLib.Interface_Steps;
