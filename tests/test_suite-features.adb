--                         Copyright (C) 2010, Sogilis                       --

with AdaSpec;
with AdaSpecLib;
with AdaSpec.Steps;
with AdaSpec.Scenarios;
with AdaSpec.Features;
with Util.IO;
with Util.Strings;

use AdaSpec;
use AdaSpecLib;
use AdaSpec.Steps;
use AdaSpec.Scenarios;
use AdaSpec.Features;
use Util.IO;
use Util.Strings;
use Util.Strings.String_Vectors;

package body Test_Suite.Features is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_1);
      Ret.Add_Test (new Test_2);
      Ret.Add_Test (new Test_3);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return String is
      pragma Unreferenced (T);
   begin
      return ("AdaSpec.Features simplest.feature");
   end Name;

   procedure Run (T : in out Test_1) is
      use Text_IO;
      Feature  : Feature_File_Type := Null_Feature_File;
      File     : constant String := "tests/features/simplest.feature";
      Scenario : Scenario_Type;
      Stanza   : Step_Type;

      CRLF : constant String := "" & ASCII.LF;
      Canonical_Feature_Text : constant String :=
         "# File: " & File              & CRLF &
         "Feature: Sample"              & CRLF &
         ""                             & CRLF &
         "  Background: "               & CRLF &
         "    Given this step works"    & CRLF &
         ""                             & CRLF &
         "  Scenario: Run a good step"  & CRLF &
         "    Given this step works"    & CRLF &
         ""                             & CRLF;

   begin

      T.Assert (not Parsed (Feature),
              "Feature has been parsed without invoking Make");

      declare
         procedure P;
         procedure P is
            S : constant String := To_String (Feature);
            pragma Unreferenced (S);
         begin
            null;
         end P;
         procedure Assert_Exception_Raised is new Assert_Except (Test_1, P);
      begin
         Assert_Exception_Raised (T,
            "1:To_String should raise Unparsed_Feature",
            Unparsed_Feature'Identity);
      end;

      Make (Feature, File);

      T.Assert (File_Name (Feature) = File,
              "Feature filename (" & File_Name (Feature) & ") is incorrect");

      T.Assert (not Parsed (Feature),
              "Feature has been parsed without invoking Parse");

      declare
         procedure P;
         procedure P is
            S : constant String := To_String (Feature);
            pragma Unreferenced (S);
         begin
            null;
         end P;
         procedure Assert_Exception_Raised is new Assert_Except (Test_1, P);
      begin
         Assert_Exception_Raised (T,
            "2:To_String should raise Unparsed_Feature",
            Unparsed_Feature'Identity);
      end;

      Parse (Feature, Std_Logger);

      T.Assert (Parsed (Feature),
              "Feature has not been parsed after invoking Parse");

      Put_Line (To_String (Feature));

      T.Assert (Feature.Name = "Sample",
              "Feature name incorrect");

      T.Assert (Feature.Description = "",
              "Feature description while there is none");

      T.Assert (Feature.Background.Name = "",
              "Background name while there is none");

      T.Assert (Feature.Background.Step_Count /= 0,
              "No background stanzas");

      T.Assert (Feature.Background.Step_Count = 1,
              "More than one line of background");

      Stanza := Feature.Background.Step_Element (0);

      T.Assert (Stanza.Kind = Step_Given,
              "The first step of the background is not a Given");

      T.Assert (Stanza.Stanza = "this step works",
              "Text of the first given of background incorrect");

      T.Assert (Feature.Scenario_Count /= 0,
              "No scenario");

      T.Assert (Feature.Scenario_Count = 1,
              "More than one scenario");

      Scenario := Feature.Scenario_Element (0);

      T.Assert (Scenario.Name = "Run a good step",
              "Name of the scenario incorrect");

      T.Assert (Scenario.Step_Count = 1,
              "No or more than one stanza in the scenario");

      Stanza := Scenario.Step_Element (0);

      T.Assert (Stanza.Kind = Step_Given,
              "The first step of the scenario is not a Given");

      T.Assert (Stanza.Stanza = "this step works",
              "Text of the first given of scenario incorrect");

      T.Assert (Feature.To_String = Canonical_Feature_Text,
                "To_String do not match the canonical text." & ASCII.LF &
                "Got:      " & Ada_String (Feature.To_String) & ASCII.LF &
                "Expected: " & Ada_String (Canonical_Feature_Text));
   end Run;

   --  Test_2  ----------------------------------------------------------------

   function  Name (T : in Test_2) return String is
      pragma Unreferenced (T);
   begin
      return ("AdaSpec.Features.Same simplest2.feature");
   end Name;

   procedure Run (T : in out Test_2) is
      use Text_IO;

      Feature1 : Feature_Type := Null_Feature;
      Feature2 : Feature_File_Type;
      File     : constant String := "tests/features/simplest2.feature";
      Scenario : Scenario_Type;

   begin

      Make (Feature1, "Sample2");

      T.Assert (Feature1.Name = "Sample2", "Incorrect feature name");
      T.Assert (Feature1.Parsed, "Feature_Type is always parsed");

      Scenario := New_Scenario ("Background");
      Step_Append (Scenario, Stanza_Given ("this step works"));
      Feature1.Set_Background (Scenario);

      Scenario := New_Scenario ("Run a good step");
      Step_Append (Scenario, Stanza_Given ("this step works"));
      Step_Append (Scenario, Stanza_Given ("I am in front of a cake machine"));
      Step_Append (Scenario, Stanza_When  ("I insert money"));
      Step_Append (Scenario, Stanza_When  ("I push the button"));
      Step_Append (Scenario, Stanza_Then  ("I get a cake"));
      Feature1.Scenario_Append (Scenario);

      Scenario := New_Scenario ("Another good step");
      Step_Append (Scenario, Stanza_Given ("this step works"));
      Feature1.Scenario_Append (Scenario);

      Make  (Feature2, File);
      Parse (Feature2, Std_Logger);

      Put_Line ("Feature1:");
      Put_Line (Feature1.To_String);
      Put_Line ("Feature2:");
      Put_Line (Feature2.To_String);

      T.Assert (Feature1.To_String = Feature_Type (Feature2).To_String,
                "The two features text representation must be the same");


   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Reraise_Occurrence (Error);

   end Run;


   --  Test_3  ----------------------------------------------------------------

   function  Name (T : in Test_3) return String is
      pragma Unreferenced (T);
   begin
      return ("AdaSpec.Features null test");
   end Name;

   procedure Run (T : in out Test_3) is
      use Text_IO;
   begin

      null;

   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Reraise_Occurrence (Error);

   end Run;

end Test_Suite.Features;

