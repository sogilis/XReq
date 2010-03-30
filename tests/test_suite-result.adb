--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers;
with Ada.Strings.Unbounded;
with Util.IO;
with AdaSpec.Lang;
with AdaSpec.Features;
with AdaSpec.Step_Definitions;
with AdaSpec.Steps;
with AdaSpec.Result;
with AdaSpecLib.String_Tables;

use Ada.Strings.Unbounded;
use Util.IO;
use AdaSpec.Lang;
use AdaSpec.Features;
use AdaSpec.Step_Definitions;
use AdaSpec.Steps;
use AdaSpec.Result;

package body Test_Suite.Result is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_Result_Step_Type);
      Ret.Add_Test (new Test_Result_Scenario_Type);
      Ret.Add_Test (new Test_Result_Scenario_Outline);
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
      Step : Result_Step_Type;
   begin

      Make (Step, "Proc", Stanza_Given (""));

      T.Assert (Procedure_Name (Step) = "Proc",
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
      use Result_Steps;
      use Ada.Containers;
      Result       : Result_Scenario_Type;
      Scenario     : Scenario_Type;
      Steps        : Step_Definitions_Type
                   := Load ("tests/features/step_definitions", Lang_Ada);
      Ideal_Result : Result_Steps.Vector;
      A, B         : Result_Step_Type;
      Errors       : Boolean;
   begin

      Make   (Scenario, "Scenario");
      Append (Scenario, Stanza_Given ("this step works"));
      Append (Scenario, Stanza_When  ("this step works too"));

      Process_Scenario (Result, Scenario, Steps, Std_Logger, Errors);

      T.Assert (not Errors, "Errors happened while processing scenario (1)");

      Append (Ideal_Result,
              AdaSpec.Result.Create ("Sample1.This_Step_Works",
                                     Stanza_Given ("this step works")));
      Append (Ideal_Result,
              AdaSpec.Result.Create ("Sample1.This_Step_Works_Too",
                                     Stanza_When  ("this step works too")));

      T.Assert (Length (Result.Steps) = 2,
              "Wrong length of result, " & Length (Result.Steps)'Img &
              " instead of 2");

      A := Element (Result.Steps, 0);
      B := Element (Ideal_Result, 0);
      T.Assert (A = B,
              "Wrong Step #0: " & To_String (A) & " /= " & To_String (B));

      A := Element (Result.Steps, 1);
      B := Element (Ideal_Result, 1);
      T.Assert (A = B,
              "Wrong Step #1: " & To_String (A) & " /= " & To_String (B));

      T.Assert (Result.Steps = Ideal_Result,
              "Wrong scenario result (1)");

      Append (Scenario, Stanza_When  ("this step doesn't work"));
      Process_Scenario (Result, Scenario, Steps, Std_Logger, Errors);
      T.Assert (Errors, "No error while processing scenario (2)");

      T.Assert (Result.Steps = Ideal_Result,
              "Wrong scenario result (2)");

      Free (Steps);
   end Run;

   --  Test_Result_Feature_Type  ----------------------------------------------

   function  Name (T : in Test_Result_Feature_Type)
                   return String is
      pragma Unreferenced (T);
   begin
      return ("AdaSpec.Result.Result_Feature_Type");
   end Name;

   procedure Run (T : in out Test_Result_Feature_Type) is
      use Result_Steps;
      use Result_Scenarios;
      CRLF     : constant String := ASCII.CR & ASCII.LF;
      R_Scen   : Result_Scenario_Type;
      Expected : Result_Feature_Type;
      Result   : Result_Feature_Type;
      Feature  : Feature_File_Ptr;
      Steps    : Step_Definitions_Type;
      Exp_Str  : constant String :=
               "Feature Sample"                     & CRLF &
               "   Background "                     & CRLF &
               "      Sample1.This_Step_Works ( );" & CRLF &
               "   End Background "                 & CRLF &
               "   Scenario Run a good step"        & CRLF &
               "      Sample1.This_Step_Works ( );" & CRLF &
               "   End Scenario Run a good step"    & CRLF &
               "End Feature Sample"                 & CRLF;
   begin

      Steps   := Load   ("tests/features/step_definitions", Lang_Ada);
      Feature := new Feature_File_Type'(Create
            ("tests/features/simplest.feature"));

      declare
         procedure P;
         procedure P is
         begin
            Process_Feature (Result, Feature_Ptr (Feature),
                             Steps, Std_Logger);
         end P;
         procedure A is new Assert_Except (Test_Result_Feature_Type, P);
      begin
         A (T, "Process_Feature should raise Unparsed_Feature",
            Unparsed_Feature'Identity);
      end;

      Parse (Feature.all, Std_Logger);

      T.Assert (Feature_Ptr (Feature).all.Name = "Sample",
              "Feature name incorrect");

      Process_Feature (Result, Feature_Ptr (Feature), Steps, Std_Logger);

      T.Assert (Result.Name = "Sample",
              "Feature name incorrect (2)");

      Append (R_Scen, Create ("Sample1.This_Step_Works",
                              Stanza_Given ("this step works")));
      Expected.Background := R_Scen;
      R_Scen.Name := To_Unbounded_String ("Run a good step");
      Append (Expected, R_Scen);
      Expected.Name := To_Unbounded_String ("Sample");

--       Can't Test the "=" operator without loading twice the same file
--
--       T.Assert (Result = Expected,
--               "Result not expected. Found:" & CRLF &
--               To_String (Result) & "Expected:" & CRLF &
--               To_String (Expected) & "---");

      T.Assert (To_String (Result) = Exp_Str,
              "To_String value not expected:" & CRLF & To_String (Result));

      Free (Steps);
   end Run;

   --  Test_To_String  --------------------------------------------------

   function  Name (T : in Test_To_String)
                   return String is
      pragma Unreferenced (T);
   begin
      return ("AdaSpec.Result.To_String");
   end Name;

   procedure Run (T : in out Test_To_String) is
      use Result_Steps;
      use Result_Scenarios;
      use Match_Vectors;
      CRLF     : constant String := ASCII.CR & ASCII.LF;
      Expected : constant String :=
               "Feature simplest feature"           & CRLF &
               "   Background BG"                   & CRLF &
               "      Sample1.This_Step_Works ((""this step works"" 1 15) );"
                                                    & CRLF &
               "   End Background BG"               & CRLF &
               "   Scenario Run a good step"        & CRLF &
               "      Sample1.This_Step_Works ((""this step works"" 1 15) );"
                                                    & CRLF &
               "   End Scenario Run a good step"    & CRLF &
               "End Feature simplest feature"       & CRLF;
      R_Scen   : Result_Scenario_Type;
      Feature  : Result_Feature_Type;
      Matches  : Match_Vectors.Vector;
   begin

      Append (Matches, (1, 15));
      Append (R_Scen, Create ("Sample1.This_Step_Works",
                              Stanza_Given ("this step works"),
                              Matches));
      Feature.Background := R_Scen;
      Feature.Background.Name := To_Unbounded_String ("BG");
      R_Scen.Name := To_Unbounded_String ("Run a good step");
      Append (Feature, R_Scen);
      Feature.Name := To_Unbounded_String ("simplest feature");

      T.Assert (To_String (Feature) = Expected,
              "To_String value not expected:" & CRLF & To_String (Feature));

   end Run;

   --  Test_Result_Scenario_Outline  ------------------------------------------

   function  Name (T : in Test_Result_Scenario_Outline)
                   return String is
      pragma Unreferenced (T);
   begin
      return ("AdaSpec.Result.Result_Scenario_Type with Scenario Outline");
   end Name;

   procedure Run (T : in out Test_Result_Scenario_Outline) is
      use Ada.Containers;
      use AdaSpecLib.String_Tables;
      use Result_Steps_Vectors2;
      use Result_Steps;
      Scenario  : Scenario_Type (True);
      Result    : Result_Scenario_Type;
      Steps     : Step_Definitions_Type
                := Load ("tests/features/step_definitions", Lang_Ada);
      Errors    : Boolean;
      Steps_tmp : Result_Steps.Vector;

      procedure Equals (Found : in Unbounded_String;
                        Expect, Description : in String);
      procedure Equals (Found : in Unbounded_String;
                        Expect, Description : in String) is
         S : constant String := To_String (Found);
      begin
         T.Assert (Found = Expect, "Expected """ & Expect & """ but found """ &
                   S & """ for " & Description);
      end Equals;
   begin

      Append (Scenario, Stanza_Given ("A is <A> and B is <B>"));
      Append (Scenario, Stanza_When  ("A is '<A>' and B is '<B>'"));
      Append (Scenario, Stanza_Then  ("C is <C>"));

      Scenario.Table.Put (0, 0, "A");
      Scenario.Table.Put (1, 0, "B");
      Scenario.Table.Put (2, 0, "C");

      Scenario.Table.Put (0, 1, "[a]");
      Scenario.Table.Put (1, 1, "[b]");
      Scenario.Table.Put (2, 1, "[c]");

      Scenario.Table.Put (0, 2, "1");
      Scenario.Table.Put (1, 2, "2");
      Scenario.Table.Put (2, 2, "3");

      Scenario.Table.Put (0, 3, "x");
      Scenario.Table.Put (1, 3, "y");
      Scenario.Table.Put (2, 3, "z");

      Process_Scenario (Result, Scenario, Steps, Std_Logger, Errors);

      T.Assert (not Errors, "Errors while Process_Scenario");

      T.Assert (Result.Outline, "The result should be an outline");
      T.Assert (Length (Result.Scenarios) = 3, "Should find 3 sub scenarios");
      T.Assert (Length (Result.Steps) = 3, "Should find 3 steps for outline" &
                ". Found" & Length (Result.Steps)'Img);

      Steps_tmp := Element (Result.Scenarios, 0);

      T.Assert (Length (Steps_tmp) = 3, "3 steps in scenario 1");
      Equals (Element (Steps_tmp, 0).Step.Stanza, "A is [a] and B is [b]",
              "1st step of 1st scenario");
      Equals (Element (Steps_tmp, 1).Step.Stanza, "A is '[a]' and B is '[b]'",
              "2nd step of 1st scenario");
      Equals (Element (Steps_tmp, 2).Step.Stanza, "C is [c]",
              "3rd step of 1st scenario");

      Steps_tmp := Element (Result.Scenarios, 1);

      T.Assert (Length (Steps_tmp) = 3, "3 steps in scenario 2");
      Equals (Element (Steps_tmp, 0).Step.Stanza, "A is 1 and B is 2",
              "1st step of 2nd scenario");
      Equals (Element (Steps_tmp, 1).Step.Stanza, "A is '1' and B is '2'",
              "2nd step of 2nd scenario");
      Equals (Element (Steps_tmp, 2).Step.Stanza, "C is 3",
              "3rd step of 2nd scenario");

      Steps_tmp := Element (Result.Scenarios, 2);

      T.Assert (Length (Steps_tmp) = 3, "3 steps in scenario 3");
      Equals (Element (Steps_tmp, 0).Step.Stanza, "A is x and B is y",
              "1st step of 3rd scenario");
      Equals (Element (Steps_tmp, 1).Step.Stanza, "A is 'x' and B is 'y'",
              "2nd step of 3rd scenario");
      Equals (Element (Steps_tmp, 2).Step.Stanza, "C is z",
              "3rd step of 3rd scenario");

      Free (Steps);

   end Run;

end Test_Suite.Result;

