--                         Copyright (C) 2010, Sogilis                       --

with XReq.CLI;

package body Test_Suite.CLI is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_Help);
   end Add_Tests;

   function Name (T : in Test_Help) return String is
      pragma Unreferenced (T);
   begin
      return ("AsaSpec.CLI.Help");
   end Name;

   procedure Run (T : in out Test_Help) is
      pragma Unreferenced (T);
   begin
      XReq.CLI.Help;
      --  TODO: what can be dont to test that it is ok ?
   end Run;

end Test_Suite.CLI;
