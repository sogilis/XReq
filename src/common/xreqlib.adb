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
with Ada.Strings;
with Ada.Text_IO;

package body XReqLib is

   procedure Check_Elaboration is
      use Ada.Text_IO;
   begin
      case Elaboration_Status is
         when 2 => null;
         when others =>
            Put_Line ("Warning: XReqLib package is not elaborated (Status:" &
                      Elaboration_Status'Img & ")");
            raise XReqLib_Not_Elaborated with
               "Status" & Elaboration_Status'Img;
      end case;
   end Check_Elaboration;

   function To_String (Pos : in Position_Type) return String is
      use Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;
      use Ada.Strings;
   begin
      return To_String (Pos.File) & ":" & Trim (Pos.Line'Img, Left);
   end To_String;

   function Position  (File : in String;
                       Line : in Natural) return Position_Type
   is
      use Ada.Strings.Unbounded;
   begin
      return Position_Type'(To_Unbounded_String (File), Line);
   end Position;

begin
   Elaboration_Status := 2;
end XReqLib;
