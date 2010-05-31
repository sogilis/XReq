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

with AUnit;
with AUnit.Test_Suites;


package Test_Suite.Result is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite);

   --  Test type
   type Test_Result_Step_Type           is new Test_Case_Type with null record;
   type Test_Result_Scenario_Type       is new Test_Case_Type with null record;
   type Test_Result_Scenario_Outline    is new Test_Case_Type with null record;
   type Test_Result_Feature_Type        is new Test_Case_Type with null record;
   type Test_To_String                  is new Test_Case_Type with null record;

   --  Operation on Test_Result_Step_Type
   function  Name (T : in     Test_Result_Step_Type)
                           return String;
   procedure Run  (T : in out Test_Result_Step_Type);

   --  Operation on Test_Result_Scenario_Type
   function  Name (T : in     Test_Result_Scenario_Type) return String;
   procedure Run  (T : in out Test_Result_Scenario_Type);

   --  Operation on Test_Result_Scenario_Outline
   function  Name (T : in     Test_Result_Scenario_Outline) return String;
   procedure Run  (T : in out Test_Result_Scenario_Outline);

   --  Operation on Test_Result_Feature_Type
   function  Name (T : in     Test_Result_Feature_Type)
                           return String;
   procedure Run  (T : in out Test_Result_Feature_Type);

   --  Operation on Test_To_String
   function  Name (T : in     Test_To_String)
                           return String;
   procedure Run  (T : in out Test_To_String);

end Test_Suite.Result;
