--                         Copyright (C) 2010, Sogilis                       --

with AUnit.Assertions;

use AUnit.Assertions;

package body Test_Suite.Generator is

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
      return "AdaSpec.Generator";
   end Name;

   procedure Run (T : in out Test_1) is
      pragma Unreferenced (T);
   begin

      Assert (True, "Missing test for AdaSpec.Generator");

   end Run;

end Test_Suite.Generator;

