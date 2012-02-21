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
with Ada.Containers.Vectors;

package XReqLib is

   XReqLib_Not_Elaborated : exception;
   procedure Check_Elaboration;

   Not_Yet_Implemented : exception;

   ----------------------
   --  String_Vectors  --
   ----------------------

   package String_Vectors is new Ada.Containers.Vectors
     (Natural,
      Ada.Strings.Unbounded.Unbounded_String,
      Ada.Strings.Unbounded."=");

   subtype String_Vector is String_Vectors.Vector;

   Empty_String_Vector : constant String_Vector := String_Vectors.Empty_Vector;

   -----------------
   --  Step_Type  --
   -----------------

   type Step_All_Kind is (Step_Null, Step_Given, Step_When, Step_Then);

   subtype Step_Kind is Step_All_Kind range Step_Given .. Step_Then;

   ---------------------
   --  Position_Type  --
   ---------------------

   type Position_Type is
      record
         File : Ada.Strings.Unbounded.Unbounded_String;
         Line : Natural := 0;
      end record;

   function To_String (Pos : in Position_Type) return String;
   function Position  (File : in String;
                       Line : in Natural) return Position_Type;

   Null_Position : constant Position_Type;

private

   Null_Position : constant Position_Type := (others => <>);

   Elaboration_Status : Natural := 1;
   --  Should be 2 after elaboration

end XReqLib;
