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

with Ada.Exceptions;
with AUnit.Test_Suites;
with AUnit.Simple_Test_Cases;

use Ada.Exceptions;

package Test_Suite is

   -------------
   --  Suite  --
   -------------

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

   ---------------
   --  Banners  --
   ---------------

   procedure Title (Text : in String);
   procedure Begin_Test (Text : in String) renames Title;
   procedure End_Test;

   --------------
   --  Output  --
   --------------

   procedure T_Put      (Item : in String);
   procedure T_Put_Line (Item : in String);
   procedure T_New_Line;

   package Text_IO is

      procedure Put      (Item : in String) renames T_Put;
      procedure Put_Line (Item : in String) renames T_Put_Line;
      procedure New_Line                    renames T_New_Line;

   end Text_IO;

   ------------------------
   --  Custom_Test_Case  --
   ------------------------

   type Test_Case_Type is abstract
      new AUnit.Simple_Test_Cases.Test_Case with null record;
   type Test_Case_Ptr is access all Test_Case_Type;

   function  Name     (T : in     Test_Case_Type) return String is abstract;
   procedure Run      (T : in out Test_Case_Type) is abstract;
   function  Name     (T : in     Test_Case_Type) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_Case_Type);

   generic
      type Test_Case_Generic_Type (<>) is
         abstract new Test_Case_Type with private;
      with procedure Proc;
   procedure Assert_Except (T       : in Test_Case_Generic_Type;
                            Message : in String;
                            Err     : in Exception_Id := Null_Id);

end Test_Suite;
