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
      A, B         : Result_Step_Type;
      Errors       : Boolean;
   begin

      Make   (Scenario, "Scenario");
      Append (Scenario, Stanza_Given ("this step works"));
      Append (Scenario, Stanza_When  ("this step works too"));

      Process_Scenario (Result, Scenario, Steps, Errors);

      Assert (not Errors, "Errors happened while processing scenario (1)");

      Append (Ideal_Result, AdaSpec.Result.Create ("Steps.This_Step_Works"));
      Append (Ideal_Result,
              AdaSpec.Result.Create ("Steps.This_Step_Works_Too"));

      Assert (Length (Result.Steps) = 2,
              "Wrong length of result, " & Length (Result.Steps)'Img &
              " instead of 2");

      A := Element (Result.Steps, 0);
      B := Element (Ideal_Result, 0);
      Assert (A = B,
              "Wrong Step #0: " & To_String (A) & " /= " & To_String (B));

      A := Element (Result.Steps, 1);
      B := Element (Ideal_Result, 1);
      Assert (A = B,
              "Wrong Step #1: " & To_String (A) & " /= " & To_String (B));

      Assert (Result.Steps = Ideal_Result,
              "Wrong scenario result (1)");

      Append (Scenario, Stanza_When  ("this step doesn't work"));
      Process_Scenario (Result, Scenario, Steps, Errors);
      Assert (Errors, "No error while processing scenario (2)");

      Assert (Result.Steps = Ideal_Result,
              "Wrong scenario result (2)");

   end Run_Test;

end Test_Suite.Result;

