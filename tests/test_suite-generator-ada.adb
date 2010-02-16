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

   function  Name (T : in Test_1) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("AdaSpec.Generator.Ada");
   end Name;

   procedure Run_Test (T : in out Test_1) is
      pragma Unreferenced (T);
   begin

      Assert (False, "Missing test for AdaSpec.Generator.Ada");

   end Run_Test;

end Test_Suite.Generator.Ada;

