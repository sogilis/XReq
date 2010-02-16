--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers;
with AUnit.Assertions;
with AdaSpec.Features;
with AdaSpec.Steps;
with AdaSpec.Stanzas;
with AdaSpec.Result;

use AUnit.Assertions;
use AdaSpec.Features;
use AdaSpec.Steps;
use AdaSpec.Stanzas;
use AdaSpec.Result;

package body Test_Suite.Result is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_Result_Step_Type);
      Ret.Add_Test (new Test_Result_Scenario_Type);
   end Add_Tests;

   --  Result_Step_Type  ------------------------------------------------------

   function  Name (T : in Test_Result_Step_Type) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("AdaSpec.Result.Result_Step_Type");
   end Name;

   procedure Run_Test (T : in out Test_Result_Step_Type) is
      pragma Unreferenced (T);
      Step : Result_Step_Type;
   begin

      Make (Step, "Proc");

      Assert (Procedure_Name (Step) = "Proc",
              "Wrong procedure name");

   end Run_Test;

   --  Result_Scenario_Type  --------------------------------------------------

   function  Name (T : in Test_Result_Scenario_Type)
                   return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("AdaSpec.Result.Result_Scenario_Type");
   end Name;

   procedure Run_Test (T : in out Test_Result_Scenario_Type) is
      pragma Unreferenced (T);
      use Result_Steps;
      use Ada.Containers;
      Result       : Result_Scenario_Type;
      Scenario     : Scenario_Type;
      Steps        : constant Steps_Type
                   := Load ("tests/features/step_definitions");
      Ideal_Result : Result_Steps.Vector;
   begin

      Make   (Scenario, "Scenario");
      Append (Scenario, Stanza_Given ("this step works"));
      Append (Scenario, Stanza_Given ("this step works too"));

      Process_Scenario (Result, Scenario, Steps);

      Append (Ideal_Result, AdaSpec.Result.Create ("This_Step_Works"));
      Append (Ideal_Result, AdaSpec.Result.Create ("This_Step_Works_Too"));

      Assert (Length (Result.Steps) = 2,
              "Wrong length of result, " & Length (Result.Steps)'Img &
              " instead of 2");

      Assert (Result.Steps = Ideal_Result,
              "Wrong scenario result");

   end Run_Test;

end Test_Suite.Result;

