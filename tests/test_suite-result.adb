-------------------------------------------------------------------------------
--  XReq  --  Behaviour Driven Developpement tool for compiled languages     --
--  Copyright (c) 2010, SOGILIS <http://sogilis.com>                         --
--                                                                           --
--  This program is free software: you can redistribute it and/or modify     --
--  it under the terms of the GNU Affero General Public License as           --
--  published by the Free Software Foundation, either version 3 of the       --
--  License, or (at your option) any later version.                          --
--                                                                           --
--  This program is distributed in the hope that it will be useful,          --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of           --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            --
--  GNU Affero General Public License for more details.                      --
--                                                                           --
--  You should have received a copy of the GNU Affero General Public License --
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.    --
--                                                                           --
-------------------------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Containers;
with Ada.Strings.Unbounded;
with Util.IO;
with Util.Strings;
with XReqLib;
with XReqLib.String_Tables;
with XReq.Lang;
with XReq.Features.Handles;
with XReq.Features.Files;
with XReq.Step_Definitions.Handles;
with XReq.Step_Definition_List.Handles;
with XReq.Scenarios;
with XReq.Scenarios.Handles;
with XReq.Steps;
with XReq.Steps.Handles;
with XReq.Steps.Result;
with XReq.Steps.Result.Handles;
with XReq.Scenarios.Result;
with XReq.Scenarios.Result.Handles;
with XReq.Features.Result.Handles;

use Ada.Strings.Unbounded;
use Util.IO;
use Util.Strings;
use XReqLib;
use XReq.Lang;
use XReq.Features.Handles;
use XReq.Features.Files;
use XReq.Step_Definitions.Handles;
use XReq.Step_Definition_List.Handles;
use XReq.Scenarios.Handles;
use XReq.Steps;
use XReq.Steps.Handles;
use XReq.Steps.Result;
use XReq.Steps.Result.Handles;
use XReq.Scenarios.Result.Handles;
use XReq.Features.Result.Handles;

package body Test_Suite.Result is

   subtype Table_Type is XReqLib.String_Tables.Table;

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
      return ("XReq.Result.Result_Step_Type");
   end Name;

   procedure Run (T : in out Test_Result_Step_Type) is
      Step : Result_Step_Type;
   begin

      Step.Make (Stanza_Given (""), (Proc_Name => To_Unbounded_String ("Proc"),
                                     others    => <>));

      T.Assert (Step.Procedure_Name = "Proc",
              "Wrong procedure name");

      Step.Set_Procedure_Name ("pp");

      T.Assert (Step.Procedure_Name = "pp",
              "Wrong procedure name (2)");

   end Run;

   --  Result_Scenario_Type  --------------------------------------------------

   function  Name (T : in Test_Result_Scenario_Type)
                   return String is
      pragma Unreferenced (T);
   begin
      return ("XReq.Result.Result_Scenario_Type");
   end Name;

   procedure Run (T : in out Test_Result_Scenario_Type) is
      package Result_Steps is new Ada.Containers.Vectors
        (Natural, Result_Step_Type, "=");
      use Result_Steps;
      use Ada.Containers;

      function Eq (S : in Result_Scenario_Handle;
                   V : in Result_Steps.Vector) return Boolean;
      function Eq (S : in Result_Scenario_Handle;
                   V : in Result_Steps.Vector) return Boolean is
         H : Result_Step_Handle;
      begin
         if Integer (Length (V)) /= S.R.Step_Count then
            return False;
         end if;
         for I in S.R.Step_First .. S.R.Step_Last loop
            H := S.R.all.Step_Element (I);
            if Result_Step_Type (H.R.all) /= Element (V, I) then
               Std_Logger.Put_Line ("Fail at index" & I'Img);
               return False;
            end if;
         end loop;
         return True;
      end Eq;

      Result        : constant Result_Scenario_Handle := Create;
      Scenario      : Scenario_Handle;
      Steps         : constant Step_File_List_Handle := Create;
      Ideal_Result  : Result_Steps.Vector;
      A             : Result_Step_Handle;
      B             : Result_Step_Type;
      Errors        : Boolean;
      Missing_Steps : String_Set;
   begin
      Steps.Ref.Load ("tests/features/step_definitions", Lang_Ada);

      Scenario := Create;
      Scenario.R.Make ("Scenario");
      Scenario.R.Step_Append (Stanza_Given ("this step works"));
      Scenario.R.Step_Append (Stanza_When  ("this step works too"));

      T.Assert (Scenario.R.Step_Count = 2,
              "Wrong length of scenario, " & Scenario.R.Step_Count'Img &
              " instead of 2");

      Result.R.Process_Scenario (Scenario,
                        Steps, Std_Logger, Errors, Missing_Steps);

      T.Assert (not Errors, "Errors happened while processing scenario (1)");

      Append (Ideal_Result, New_Result_Step
              (Stanza_Given ("this step works"),
               Steps.Ref.all.Find (Stanza_Given ("this step works"))));
      Append (Ideal_Result, New_Result_Step
              (Stanza_When ("this step works too"),
               Steps.Ref.all.Find (Stanza_When  ("this step works too"))));

      T.Assert (Result.R.Step_Count = 2,
              "Wrong length of result, " & Result.R.Step_Count'Img &
              " instead of 2");

      A := Result.R.all.Step_Element (0);
      B := Element (Ideal_Result, 0);
      T.Assert (A.R.Procedure_Name = "Sample1.This_Step_Works",
                "Wrong Step #0: " & A.R.To_Code);
      declare
         AA : constant Step_Type := Step_Type (A.R.all);
         BB : constant Step_Type := Step_Type (B);
      begin
         T.Assert (Equals (AA, BB),
                 "Wrong Step #0 Step_Type (Equals): """ &
                 AA.To_String & """ /= """ & BB.To_String & """ OR " &
                 To_String (AA.Position) & " /= " & To_String (BB.Position) &
                 " OR" & AA.Arg_Count'Img & " /=" & BB.Arg_Count'Img);
         T.Assert (AA = BB,
                 "Wrong Step #0 Step_Type (""=""): """ &
                 AA.To_String & """ /= """ & BB.To_String & """ OR " &
                 To_String (AA.Position) & " /= " & To_String (BB.Position) &
                 " OR" & AA.Arg_Count'Img & " /=" & BB.Arg_Count'Img);
      end;
      T.Assert (Result_Step_Type (A.R.all) = B,
              "Wrong Step #0 Result_Step_Type (""=""): " &
              A.R.To_Code & " /= " & B.To_Code);

      A := Result.R.all.Step_Element (1);
      B := Element (Ideal_Result, 1);
      T.Assert (A.R.Procedure_Name = "Sample1.This_Step_Works_Too",
                "Wrong Step #1: " & A.R.To_Code);
      T.Assert (Result_Step_Type (A.R.all) = B,
              "Wrong Step #1: " & A.R.To_Code & " /= " & B.To_Code);

      T.Assert (Integer (Length (Ideal_Result)) = Result.R.Step_Count,
              "Wrong scenario result length (1)" & Result.R.Step_Count'Img &
              " instead of" & Length (Ideal_Result)'Img);

      T.Assert (Eq (Result, Ideal_Result),
              "Wrong scenario result (1)");

      Scenario.R.Step_Append (Stanza_When  ("this step doesn't work"));
      Result.R.Process_Scenario
        (Scenario, Steps, Std_Logger, Errors, Missing_Steps);
      T.Assert (Errors, "No error while processing scenario (2)");

      T.Assert (Integer (Length (Ideal_Result)) = Result.R.Step_Count,
              "Wrong scenario result length (2)");

      T.Assert (Eq (Result, Ideal_Result),
              "Wrong scenario result (2)");
   end Run;

   --  Test_Result_Feature_Type  ----------------------------------------------

   function  Name (T : in Test_Result_Feature_Type)
                   return String is
      pragma Unreferenced (T);
   begin
      return ("XReq.Result.Result_Feature_Type");
   end Name;

   procedure Run (T : in out Test_Result_Feature_Type) is
      CRLF     : constant String := "" & ASCII.LF;
      R_Scen   : constant Result_Scenario_Handle := Create;
      Expected : constant Result_Feature_Handle := Create;
      Result   : constant Result_Feature_Handle := Create;
      Feature  : Feature_File_Ptr;
      Feature2 : Feature_Handle;
      Steps    : constant Step_File_List_Handle := Create;
      Exp_Str  : constant String :=
               "Feature Sample"                     & CRLF &
               "   Background "                     & CRLF &
               "      Sample1.This_Step_Works ( );" & CRLF &
               "   End Background "                 & CRLF &
               "   Scenario Run a good step"        & CRLF &
               "      Sample1.This_Step_Works ( );" & CRLF &
               "   End Scenario Run a good step"    & CRLF &
               "   Scenario Another one"            & CRLF &
               "      Sample2.When_I_Match ((""toto"" 10 13) );" & CRLF &
               "      Sample2.When_I_Match ((""tata"" 10 13)"    &
                                           "(""tutu"" 21 24) );" & CRLF &
               "   End Scenario Another one"        & CRLF &
               "End Feature Sample"                 & CRLF;
      Missing_Steps : String_Set;
   begin

      Steps.R.Load   ("tests/features/step_definitions", Lang_Ada);
      Feature := new Feature_File_Type'(Create
            ("tests/features/sample.feature"));
      Feature2.Set (XReq.Features.Feature_Ptr (Feature));

      declare
         procedure P;
         procedure P is
         begin
            Result.R.Process_Feature
              (Feature2, Steps, Std_Logger, Missing_Steps);
         end P;
         procedure A is new Assert_Except (Test_Result_Feature_Type, P);
      begin
         A (T, "Process_Feature should raise Unparsed_Feature",
            XReq.Features.Unparsed_Feature'Identity);
      end;

      Parse (Feature.all, Std_Logger);

      T.Assert (Feature2.R.Name = "Sample",
              "Feature name incorrect");

      Result.R.Process_Feature (Feature2, Steps, Std_Logger, Missing_Steps);

      T.Assert (Result.R.Name = "Sample",
              "Feature name incorrect (2)");

      R_Scen.R.Step_Append (Create
        (Stanza_Given ("this step works"),
         (Proc_Name => To_Unbounded_String ("Sample1.This_Step_Works"),
          others    => <>)));
      Expected.R.Set_Background (R_Scen);
      R_Scen.R.Set_Name ("Run a good step");
      Expected.R.Scenario_Append (R_Scen);
      Expected.R.Set_Name ("Sample");

--       Can't Test the "=" operator without loading twice the same file
--
--       T.Assert (Result = Expected,
--               "Result not expected. Found:" & CRLF &
--               To_String (Result) & "Expected:" & CRLF &
--               To_String (Expected) & "---");

      T.Assert (Result.R.To_Code = Exp_Str,
              "To_String value not expected:" & CRLF &
              "Got:      <<<" & Result.R.To_Code & ">>>" & CRLF &
              "Expected: <<<" & Exp_Str & ">>>");

   end Run;

   --  Test_To_String  --------------------------------------------------

   function  Name (T : in Test_To_String)
                   return String is
      pragma Unreferenced (T);
   begin
      return ("XReq.Result.To_String");
   end Name;

   procedure Run (T : in out Test_To_String) is
      use Step_Match_Vectors;
      use XReq;
      CRLF     : constant String := "" & ASCII.LF;
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
      R_Scen   : Result_Scenario_Handle := Create;
      Feature  : constant Result_Feature_Handle := Create;
      Matches  : Step_Match_Vectors.Vector;
   begin

      Append (Matches, (1, 15));
      R_Scen.R.Step_Append (Create
        (Stanza_Given ("this step works"),
         (Proc_Name => To_Unbounded_String ("Sample1.This_Step_Works"),
          Matches   => Matches,
          others    => <>)));
      R_Scen.R.Set_Name ("BG");
      Feature.R.Set_Background (R_Scen);

      R_Scen.Set_New (Scenarios.Result.Result_Scenario_Type (R_Scen.R.all));
      R_Scen.R.Set_Name ("Run a good step");
      Feature.R.Scenario_Append (R_Scen);
      Feature.R.Set_Name ("simplest feature");

      T.Assert (Feature.R.To_Code = Expected,
              "To_String value not expected:" & CRLF &
              "Got:      <<<" & Feature.R.To_Code & ">>>" & CRLF &
              "Expected: <<<" & Expected & ">>>");

   end Run;

   --  Test_Result_Scenario_Outline  ------------------------------------------

   function  Name (T : in Test_Result_Scenario_Outline)
                   return String is
      pragma Unreferenced (T);
   begin
      return ("XReq.Result.Result_Scenario_Type with Scenario Outline");
   end Name;

   procedure Run (T : in out Test_Result_Scenario_Outline) is
      use Ada.Containers;
      use XReqLib.String_Tables;
      Scenario  : Scenario_Handle;
      Result    : constant Result_Scenario_Handle := Create;
      Steps     : constant Step_File_List_Handle := Create;
      Errors    : Boolean;
      I         : Natural;
      Table     : Table_Type;
      Missing_Steps : String_Set;

      procedure Equals (Found, Expect, Description : in String);
      procedure Equals (Found, Expect, Description : in String) is
      begin
         T.Assert (Found = Expect, "Expected """ & Expect & """ but found """ &
                   Found & """ for " & Description);
      end Equals;
   begin
      Scenario.Set_New (XReq.Scenarios.Null_Scenario_Outline);
      Steps.R.Load ("tests/features/step_definitions", Lang_Ada);

      Scenario.R.Step_Append (Stanza_Given ("A is <A> and B is <B>"));
      Scenario.R.Step_Append (Stanza_When  ("A is '<A>' and B is '<B>'"));
      Scenario.R.Step_Append (Stanza_Then  ("C is <C>"));

      Table := Scenario.R.Table;

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

      Scenario.R.Set_Table (Table);

      Result.R.Process_Scenario
        (Scenario, Steps, Std_Logger, Errors, Missing_Steps);

      T.Assert (not Errors, "Errors while Process_Scenario");

      T.Assert (Result.R.Outline, "The result should be an outline");
      T.Assert (Result.R.Outline_Count = 3, "Should find 3 sub scenarios");
      T.Assert (Result.R.Step_Count = 3, "Should find 3 steps for outline" &
                ". Found" & Result.R.Step_Count'Img);

      I := 0;

      T.Assert (Result.R.Outline_Step_Count (I) = 3, "3 steps in scenario 1");
      Equals (Result.R.Outline_Step_Element (I, 0).R.Stanza,
              "A is [a] and B is [b]",
              "1st step of 1st scenario");
      Equals (Result.R.Outline_Step_Element (I, 1).R.Stanza,
              "A is '[a]' and B is '[b]'",
              "2nd step of 1st scenario");
      Equals (Result.R.Outline_Step_Element (I, 2).R.Stanza,
              "C is [c]",
              "3rd step of 1st scenario");

      I := 1;

      T.Assert (Result.R.Outline_Step_Count (I) = 3,
                Result.R.Outline_Step_Count (I)'Img &
                " steps in scenario 2 instead of 3");
      Equals (Result.R.Outline_Step_Element (I, 0).R.Stanza,
              "A is 1 and B is 2",
              "1st step of 2nd scenario");
      Equals (Result.R.Outline_Step_Element (I, 1).R.Stanza,
              "A is '1' and B is '2'",
              "2nd step of 2nd scenario");
      Equals (Result.R.Outline_Step_Element (I, 2).R.Stanza,
              "C is 3",
              "3rd step of 2nd scenario");

      I := 2;

      T.Assert (Result.R.Outline_Step_Count (I) = 3, "3 steps in scenario 3");
      Equals (Result.R.Outline_Step_Element (I, 0).R.Stanza,
              "A is x and B is y",
              "1st step of 3rd scenario");
      Equals (Result.R.Outline_Step_Element (I, 1).R.Stanza,
              "A is 'x' and B is 'y'",
              "2nd step of 3rd scenario");
      Equals (Result.R.Outline_Step_Element (I, 2).R.Stanza,
              "C is z",
              "3rd step of 3rd scenario");


   end Run;

end Test_Suite.Result;
