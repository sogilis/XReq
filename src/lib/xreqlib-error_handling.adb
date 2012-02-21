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

with GNAT.Traceback.Symbolic;

package body XReqLib.Error_Handling is

   function Exception_To_String (E : Exception_Occurrence) return String is
      Basic_Information : constant String
        := Exception_Name (E) & ": " & Exception_Message (E) & ASCII.LF;
   begin
      if Show_Traces then
         return Basic_Information & ASCII.LF
           & "Stack Trace:" & ASCII.LF & Symbolic_Traceback (E);
      else
         return Basic_Information;
      end if;
   end Exception_To_String;

   function Symbolic_Traceback (E : Exception_Occurrence) return String is
   begin
      return GNAT.Traceback.Symbolic.Symbolic_Traceback (E);
      --  TODO: GNAT.Traceback.Symbolic creates an undefined reference to the
      --        the symbol: gnat__traceback__symbolic__symbolic_traceback__2
      --        at random
      --  return Exception_Information (E);
   end Symbolic_Traceback;

end XReqLib.Error_Handling;
