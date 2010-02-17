--                         Copyright (C) 2010, Sogilis                       --

with AUnit.Assertions;
with AdaSpec.Steps;
with AdaSpec.Stanzas;

use AUnit.Assertions;
use AdaSpec.Steps;
use AdaSpec.Stanzas;

package body Test_Suite.Steps is

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
      return ("AdaSpec.Steps");
   end Name;

   procedure Run (T : in out Test_1) is
      pragma Unreferenced (T);
      Steps : Steps_Type;
      Dir   : constant String := "tests/features/step_definitions";
   begin

      Load (Steps, Dir);

      Assert (Contains (Steps, Stanza_Given ("this step works")),
              Dir & " should contains `Given this step works'");

      Assert (Contains (Steps, Stanza_When ("this step works too")),
              Dir & " should contains `When this step works too'");

      Assert (Find (Steps, Stanza_When ("this step works too")) =
              "Steps.This_Step_Works_Too",
              "`When this step works too' and link " &
              "to procedure This_Step_Works_Too");

      Assert (not Contains (Steps, Stanza_Then ("this step doesn't works")),
              Dir & " should not contains `Then this step doesn't works'");

   end Run;

end Test_Suite.Steps;

