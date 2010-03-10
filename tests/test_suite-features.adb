--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with AdaSpec;
with AdaSpec.Stanzas;
with AdaSpec.Features;
with Util.Strings;

use Ada.Strings.Unbounded;
use AdaSpec;
use AdaSpec.Stanzas;
use AdaSpec.Stanzas.Stanza_Container;
use AdaSpec.Features;
use AdaSpec.Features.Scenario_Container;
use Util.Strings.Vectors;

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
      Feature  : Feature_File_Type;
      File     : constant String := "tests/features/simplest.feature";
      Scenario : Scenario_Type;
      Stanza   : Stanza_Type;
      Errors   : Boolean;

      CRLF : constant String := ASCII.CR & ASCII.LF;
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

      Parse (Feature, Errors);

      T.Assert (not Errors, "Parse error");

      T.Assert (Parsed (Feature),
              "Feature has not been parsed after invoking Parse");

      Put_Line (To_String (Feature));

      T.Assert (Feature.Name = "Sample",
              "Feature name incorrect");

      T.Assert (Integer (Length (Feature.Description)) = 0,
              "Feature description while there is none");

      T.Assert (To_String (Feature.Background.Name) = "",
              "Background name while there is none");

      T.Assert (Integer (Length (Feature.Background.Stanzas)) /= 0,
              "No background stanzas");

      T.Assert (Integer (Length (Feature.Background.Stanzas)) = 1,
              "More than one line of background");

      Stanza := Feature.Background.Stanzas.Element (0);

      T.Assert (Stanza.Prefix = Prefix_Given,
              "The first step of the background is not a Given");

      T.Assert (To_String (Stanza.Stanza) = "this step works",
              "Text of the first given of background incorrect");

      T.Assert (Integer (Length (Feature.Scenarios)) /= 0,
              "No scenario");

      T.Assert (Integer (Length (Feature.Scenarios)) = 1,
              "More than one scenario");

      Scenario := Feature.Scenarios.Element (0);

      T.Assert (To_String (Scenario.Name) = "Run a good step",
              "Name of the scenario incorrect");

      T.Assert (Integer (Length (Scenario.Stanzas)) = 1,
              "No or more than one stanza in the scenario");

      Stanza := Scenario.Stanzas.Element (0);

      T.Assert (Stanza.Prefix = Prefix_Given,
              "The first step of the scenario is not a Given");

      T.Assert (To_String (Stanza.Stanza) = "this step works",
              "Text of the first given of scenario incorrect");

      T.Assert (To_String (Feature) = Canonical_Feature_Text,
              "To_String do not match the canonical text");
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
      Errors   : Boolean;

   begin

      Make (Feature1, "Sample2");

      T.Assert (Name   (Feature1) = "Sample2", "Incorrect feature name");
      T.Assert (Parsed (Feature1), "Feature_Type is always parsed");

      Make   (Scenario, "Background");
      Append (Scenario, Stanza_Given ("this step works"));
      Set_Background (Feature1, Scenario);

      Make   (Scenario, "Run a good step");
      Append (Scenario, Stanza_Given ("this step works"));
      Append (Scenario, Stanza_Given ("I am in front of a cake machine"));
      Append (Scenario, Stanza_When  ("I insert money"));
      Append (Scenario, Stanza_When  ("I push the button"));
      Append (Scenario, Stanza_Then  ("I get a cake"));
      Append (Feature1, Scenario);

      Make   (Scenario, "Another good step");
      Append (Scenario, Stanza_Given ("this step works"));
      Append (Feature1, Scenario);

      Make  (Feature2, File);
      Parse (Feature2, Errors);

      T.Assert (not Errors, "Parse error");

      Put_Line ("Feature1:");
      Put_Line (To_String (Feature1));
      Put_Line ("Feature2:");
      Put_Line (To_String (Feature2));

      T.Assert (To_String (Feature1) = To_String (Feature_Type (Feature2)),
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

