--                         Copyright (C) 2010, Sogilis                       --

with AdaSpecLib.Report;

use AdaSpecLib.Report;

package body Test_Suite.Lib.Report is

   procedure Add_Tests (Ret : in AUnit.Test_Suites.Access_Test_Suite) is
   begin
      Ret.Add_Test (new Test_1);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return String is
      pragma Unreferenced (T);
   begin
      return "AdaSpecLib.Report";
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

