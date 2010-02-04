--                         Copyright (C) 2010, Sogilis                       --

with AdaSpec.CLI;

--  with AUnit.Assertions; use AUnit.Assertions;

package body Test_Suite.CLI is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_Help);
   end Add_Tests;

   function Name (T : in Test_Help) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("AsaSpec.CLI.Help");
   end Name;

   procedure Run_Test (T : in out Test_Help) is
      pragma Unreferenced (T);
   begin
      AdaSpec.CLI.Help;
      --  TODO: what can be dont to test that it is ok ?
   end Run_Test;

end Test_Suite.CLI;
