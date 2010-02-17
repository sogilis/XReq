--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers;
with Ada.Strings.Unbounded;
with AUnit.Assertions;
with AdaSpec.Features;
with AdaSpec.Steps;
with AdaSpec.Stanzas;
with AdaSpec.Result;

use AUnit.Assertions;
use Ada.Strings.Unbounded;
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
      Ret.Add_Test (new Test_Result_Feature_Type);
      Ret.Add_Test (new Test_To_String);
   end Add_Tests;

   --  Result_Step_Type  ------------------------------------------------------

   function  Name (T : in Test_Result_Step_Type) return String is
      pragma Unreferenced (T);
   begin
      return ("AdaSpec.Result.Result_Step_Type");
   end Name;

   procedure Run (T : in out Test_Result_Step_Type) is
      pragma Unreferenced (T);
      Step : Result_Step_Type;
   begin

      Make (Step, "Proc");

      Assert (Procedure_Name (Step) = "Proc",
              "Wrong procedure name");

   end Run;

   --  Result_Scenario_Type  --------------------------------------------------

   function  Name (T : in Test_Result_Scenario_Type)
                   return String is
      pragma Unreferenced (T);
   begin
      return ("AdaSpec.Result.Result_Scenario_Type");
   end Name;

   procedure Run (T : in out Test_Result_Scenario_Type) is
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
      Title ("AdaSpec.Result.Result_Scenario_Type");

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

      End_Test;
   end Run;

   --  Test_Result_Feature_Type  ----------------------------------------------

   function  Name (T : in Test_Result_Feature_Type)
                   return String is
      pragma Unreferenced (T);
   begin
      return ("AdaSpec.Result.Result_Feature_Type");
   end Name;

   procedure Run (T : in out Test_Result_Feature_Type) is
      pragma Unreferenced (T);
      use Result_Steps;
      use Result_Scenarios;
      CRLF     : constant String := ASCII.CR & ASCII.LF;
      R_Scen   : Result_Scenario_Type;
      Expected : Result_Feature_Type;
      Result   : Result_Feature_Type;
      Feature  : Feature_File_Ptr;
      Steps    : Steps_Type;
      Exp_Str  : constant String :=
               "Feature Sample"                    & CRLF &
               "   Background "                    & CRLF &
               "      Steps.This_Step_Works"       & CRLF &
               "   End Background "                & CRLF &
               "   Scenario Run a good step"       & CRLF &
               "      Steps.This_Step_Works"       & CRLF &
               "   End Scenario Run a good step"   & CRLF &
               "End Feature Sample"                & CRLF;
   begin

      Title ("AdaSpec.Result.Result_Feature_Type");

      Steps   := Load   ("tests/features/step_definitions");
      Feature := new Feature_File_Type'(Create
            ("tests/features/simplest.feature"));

      declare
         procedure P;
         procedure P is
         begin
            Process_Feature (Result, Feature_Ptr (Feature), Steps);
         end P;
         procedure Assert_Exception_Raised is new Assert_Exception (P);
      begin
         Assert_Exception_Raised ("Process_Feature should raise " &
                                  "Unparsed_Feature");
      end;

      Parse (Feature.all);

      Assert (Feature_Ptr (Feature).all.Name = "Sample",
              "Feature name incorrect");

      Process_Feature (Result, Feature_Ptr (Feature), Steps);

      Assert (Result.Name = "Sample",
              "Feature name incorrect (2)");

      Append (R_Scen, Create ("Steps.This_Step_Works"));
      Expected.Background := R_Scen;
      R_Scen.Name := To_Unbounded_String ("Run a good step");
      Append (Expected, R_Scen);
      Expected.Name := To_Unbounded_String ("Sample");

      Assert (Result = Expected,
              "Result not expected. Found:" & CRLF &
              To_String (Result) & "Expected:" & CRLF &
              To_String (Expected) & "---");

      Assert (To_String (Result) = Exp_Str,
              "To_String value not expected:" & CRLF & To_String (Result));

      End_Test;
   end Run;

   --  Test_To_String  --------------------------------------------------

   function  Name (T : in Test_To_String)
                   return String is
      pragma Unreferenced (T);
   begin
      return ("AdaSpec.Result.To_String");
   end Name;

   procedure Run (T : in out Test_To_String) is
      pragma Unreferenced (T);
      use Result_Steps;
      use Result_Scenarios;
      CRLF     : constant String := ASCII.CR & ASCII.LF;
      Expected : constant String :=
               "Feature simplest feature"          & CRLF &
               "   Background BG"                  & CRLF &
               "      Steps.This_Step_Works"       & CRLF &
               "   End Background BG"              & CRLF &
               "   Scenario Run a good step"       & CRLF &
               "      Steps.This_Step_Works"       & CRLF &
               "   End Scenario Run a good step"   & CRLF &
               "End Feature simplest feature"      & CRLF;
      R_Scen   : Result_Scenario_Type;
      Feature  : Result_Feature_Type;
   begin
      Title ("AdaSpec.Result.To_String");

      Append (R_Scen, Create ("Steps.This_Step_Works"));
      Feature.Background := R_Scen;
      Feature.Background.Name := To_Unbounded_String ("BG");
      R_Scen.Name := To_Unbounded_String ("Run a good step");
      Append (Feature, R_Scen);
      Feature.Name := To_Unbounded_String ("simplest feature");

      Assert (To_String (Feature) = Expected,
              "To_String value not expected:" & CRLF & To_String (Feature));

      End_Test;
   end Run;

end Test_Suite.Result;

