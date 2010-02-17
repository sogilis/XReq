--                         Copyright (C) 2010, Sogilis                       --

with AUnit.Assertions;

use AUnit.Assertions;

package body Test_Suite.Generator.Ada is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_1);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return String is
      pragma Unreferenced (T);
   begin
      return ("AdaSpec.Generator.Ada");
   end Name;

   procedure Run (T : in out Test_1) is
      pragma Unreferenced (T);
   begin

      --  In directory tests/features/tests, test using the command:
      --  gnatmake -c -aI../step_definitions generated_file.adb

      Assert (False, "Missing test for AdaSpec.Generator.Ada");

   end Run;

end Test_Suite.Generator.Ada;

