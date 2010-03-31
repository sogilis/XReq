--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers.Vectors;
with Ada.Containers;
with Util.IO;
with AdaSpecLib.String_Tables;
with AdaSpec.Lang;
with AdaSpec.Features;
with AdaSpec.Step_Definitions;
with AdaSpec.Scenarios;
with AdaSpec.Steps;
with AdaSpec.Result_Steps;
with AdaSpec.Result_Scenarios;
with AdaSpec.Result_Features;

use Util.IO;
use AdaSpec.Lang;
use AdaSpec.Features;
use AdaSpec.Step_Definitions;
use AdaSpec.Scenarios;
use AdaSpec.Steps;
use AdaSpec.Result_Steps;
use AdaSpec.Result_Scenarios;
use AdaSpec.Result_Features;

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

      Step := New_Result_Step (Stanza_Given (""), "Proc");

      T.Assert (Step.Procedure_Name = "Proc",
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
      package Result_Steps is new Ada.Containers.Vectors
        (Natural, Result_Step_Type, Equals);
      use Result_Steps;

      function Eq (S : in Result_Scenario_Type;
                   V : in Result_Steps.Vector) return Boolean;
      function Eq (S : in Result_Scenario_Type;
                   V : in Result_Steps.Vector) return Boolean is
         use Ada.Containers;
      begin
         if Integer (Length (V)) /= S.Step_Count then
            return False;
         end if;
         for I in S.Step_First .. S.Step_Last loop
            if AdaSpec.Result_Steps.Equals
                     (S.Step_Element (I), Element (V, I))
            then
               return False;
            end if;
         end loop;
         return True;
      end Eq;

      Result       : Result_Scenario_Type;
      Scenario     : Scenario_Type;
      Steps        : Step_Definitions_Type
                   := Load ("tests/features/step_definitions", Lang_Ada);
      Ideal_Result : Result_Steps.Vector;
      A, B         : Result_Step_Type;
      Errors       : Boolean;
   begin

      Scenario := New_Scenario ("Scenario");
      Step_Append (Scenario, Stanza_Given ("this step works"));
      Step_Append (Scenario, Stanza_When  ("this step works too"));

      Process_Scenario (Result, Scenario, Steps, Std_Logger, Errors);

      T.Assert (not Errors, "Errors happened while processing scenario (1)");

      Append (Ideal_Result,
              New_Result_Step (Stanza_Given ("this step works"),
                               "Sample1.This_Step_Works"));
      Append (Ideal_Result,
              New_Result_Step (Stanza_When ("this step works too"),
                               "Sample1.This_Step_Works_Too"));

      T.Assert (Result.Step_Count = 2,
              "Wrong length of result, " & Result.Step_Count'Img &
              " instead of 2");

      A := Result.Step_Element (0);
      B := Element (Ideal_Result, 0);
      T.Assert (A = B,
              "Wrong Step #0: " & A.To_Code & " /= " & B.To_Code);

      A := Result.Step_Element (1);
      B := Element (Ideal_Result, 1);
      T.Assert (A = B,
              "Wrong Step #1: " & A.To_Code & " /= " & B.To_Code);

      T.Assert (Eq (Result, Ideal_Result),
              "Wrong scenario result (1)");

      Step_Append (Scenario, Stanza_When  ("this step doesn't work"));
      Process_Scenario (Result, Scenario, Steps, Std_Logger, Errors);
      T.Assert (Errors, "No error while processing scenario (2)");

      T.Assert (Eq (Result, Ideal_Result),
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

      Append (R_Scen, New_Result_Step (Stanza_Given ("this step works"),
                                       "Sample1.This_Step_Works"));
      Expected.Set_Background (R_Scen);
      R_Scen.Set_Name ("Run a good step");
      Append (Expected, R_Scen);
      Expected.Set_Name ("Sample");

--       Can't Test the "=" operator without loading twice the same file
--
--       T.Assert (Result = Expected,
--               "Result not expected. Found:" & CRLF &
--               To_String (Result) & "Expected:" & CRLF &
--               To_String (Expected) & "---");

      T.Assert (Result.To_Code = Exp_Str,
              "To_String value not expected:" & CRLF & Result.To_Code);

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
      Append (R_Scen, New_Result_Step (Stanza_Given ("this step works"),
                                       "Sample1.This_Step_Works",
                                       Matches));
      R_Scen.Set_Name ("BG");
      Feature.Set_Background (R_Scen);
      R_Scen.Set_Name ("Run a good step");
      Append (Feature, R_Scen);
      Feature.Set_Name ("simplest feature");

      T.Assert (Feature.To_Code = Expected,
              "To_String value not expected:" & CRLF & Feature.To_Code);

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
      Scenario  : Scenario_Type := Null_Scenario_Outline;
      Result    : Result_Scenario_Type;
      Steps     : Step_Definitions_Type
                := Load ("tests/features/step_definitions", Lang_Ada);
      Errors    : Boolean;
      I         : Natural;
      Table     : Table_Type;

      procedure Equals (Found, Expect, Description : in String);
      procedure Equals (Found, Expect, Description : in String) is
      begin
         T.Assert (Found = Expect, "Expected """ & Expect & """ but found """ &
                   Found & """ for " & Description);
      end Equals;
   begin

      Step_Append (Scenario, Stanza_Given ("A is <A> and B is <B>"));
      Step_Append (Scenario, Stanza_When  ("A is '<A>' and B is '<B>'"));
      Step_Append (Scenario, Stanza_Then  ("C is <C>"));

      Table := Scenario.Table;

      Table.Put (0, 0, "A");
      Table.Put (1, 0, "B");
      Table.Put (2, 0, "C");

      Table.Put (0, 1, "[a]");
      Table.Put (1, 1, "[b]");
      Table.Put (2, 1, "[c]");

      Table.Put (0, 2, "1");
      Table.Put (1, 2, "2");
      Table.Put (2, 2, "3");

      Table.Put (0, 3, "x");
      Table.Put (1, 3, "y");
      Table.Put (2, 3, "z");

      Scenario.Set_Table (Table);

      Process_Scenario (Result, Scenario, Steps, Std_Logger, Errors);

      T.Assert (not Errors, "Errors while Process_Scenario");

      T.Assert (Result.Outline, "The result should be an outline");
      T.Assert (Result.Outline_Count = 3, "Should find 3 sub scenarios");
      T.Assert (Result.Step_Count = 3, "Should find 3 steps for outline" &
                ". Found" & Result.Step_Count'Img);

      I := 0;

      T.Assert (Result.Outline_Step_Count (I) = 3, "3 steps in scenario 1");
      Equals (Result.Outline_Step_Element (I, 0).Stanza,
              "A is [a] and B is [b]",
              "1st step of 1st scenario");
      Equals (Result.Outline_Step_Element (I, 1).Stanza,
              "A is '[a]' and B is '[b]'",
              "2nd step of 1st scenario");
      Equals (Result.Outline_Step_Element (I, 2).Stanza,
              "C is [c]",
              "3rd step of 1st scenario");

      I := 1;

      T.Assert (Result.Outline_Step_Count (I) = 3, "3 steps in scenario 2");
      Equals (Result.Outline_Step_Element (I, 0).Stanza,
              "A is 1 and B is 2",
              "1st step of 2nd scenario");
      Equals (Result.Outline_Step_Element (I, 1).Stanza,
              "A is '1' and B is '2'",
              "2nd step of 2nd scenario");
      Equals (Result.Outline_Step_Element (I, 2).Stanza,
              "C is 3",
              "3rd step of 2nd scenario");

      I := 2;

      T.Assert (Result.Outline_Step_Count (I) = 3, "3 steps in scenario 3");
      Equals (Result.Outline_Step_Element (I, 0).Stanza,
              "A is x and B is y",
              "1st step of 3rd scenario");
      Equals (Result.Outline_Step_Element (I, 1).Stanza,
              "A is 'x' and B is 'y'",
              "2nd step of 3rd scenario");
      Equals (Result.Outline_Step_Element (I, 2).Stanza,
              "C is z",
              "3rd step of 3rd scenario");

      Free (Steps);

   end Run;

end Test_Suite.Result;

