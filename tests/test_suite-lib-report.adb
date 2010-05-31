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

with XReqLib.Report;

use XReqLib.Report;

package body Test_Suite.Lib.Report is

   procedure Add_Tests (Ret : in AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Ret.Add_Test (new Test_1);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return String is
      pragma Unreferenced (T);
   begin
      return "XReqLib.Report";
   end Name;

   procedure Run (T : in out Test_1) is
      Report  : constant Report_Type := (others => <>);
      Report2 : constant Report_Type := (Count_Scenario_Failed => 1,
                                         others => <>);
      Report3 : constant Report_Type := (Count_Steps_Failed => 1,
                                         others => <>);
      Report4 : constant Report_Type := (Count_Steps_Skipped => 1,
                                         others => <>);
   begin

      T.Assert (Status (Report), "Report should succeed");
      T.Assert (not Status (Report2), "Report2 should fail");
      T.Assert (not Status (Report3), "Report3 should fail");
      T.Assert (not Status (Report4), "Report4 should fail");

   end Run;

end Test_Suite.Lib.Report;
