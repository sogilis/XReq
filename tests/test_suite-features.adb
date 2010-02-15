--                         Copyright (C) 2010, Sogilis                       --

with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with AUnit.Assertions;
with AdaSpec;
with AdaSpec.Stanzas;
with AdaSpec.Features;
with Util.Strings;

use Ada.Exceptions;
use Ada.Strings.Unbounded;
use AUnit.Assertions;
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

   function  Name (T : in Test_1) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("AdaSpec.Features simplest.feature");
   end Name;

   procedure Run_Test (T : in out Test_1) is
      use Ada.Text_IO;
      pragma Unreferenced (T);
      Feature  : Feature_File_Type;
      File     : constant String := "tests/features/simplest.feature";
      Scenario : Scenario_Type;
      Stanza   : Stanza_Type;

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

      Assert (not Parsed (Feature),
              "Feature has been parsed without invoking Make");

      declare
         procedure P;
         procedure P is
            S : constant String := To_String (Feature);
            pragma Unreferenced (S);
         begin
            null;
         end P;
         procedure Assert_Exception_Raised is new Assert_Exception (P);
      begin
         Assert_Exception_Raised ("1:To_String should raise Unparsed_Feature");
      end;

      Make (Feature, File);

      Assert (File_Name (Feature) = File,
              "Feature filename (" & File_Name (Feature) & ") is incorrect");

      Assert (not Parsed (Feature),
              "Feature has been parsed without invoking Parse");

      declare
         procedure P;
         procedure P is
            S : constant String := To_String (Feature);
            pragma Unreferenced (S);
         begin
            null;
         end P;
         procedure Assert_Exception_Raised is new Assert_Exception (P);
      begin
         Assert_Exception_Raised ("2:To_String should raise Unparsed_Feature");
      end;

      Parse (Feature);

      Assert (Parsed (Feature),
              "Feature has not been parsed after invoking Parse");

      Put_Line (To_String (Feature));

      Assert (Feature.Name = "Sample",
              "Feature name incorrect");

      Assert (Integer (Length (Feature.Description)) = 0,
              "Feature description while there is none");

      Assert (To_String (Feature.Background.Name) = "",
              "Background name while there is none");

      Assert (Integer (Length (Feature.Background.Stanzas)) /= 0,
              "No background stanzas");

      Assert (Integer (Length (Feature.Background.Stanzas)) = 1,
              "More than one line of background");

      Stanza := Feature.Background.Stanzas.Element (0);

      Assert (Stanza.Prefix = Prefix_Given,
              "The first step of the background is not a Given");

      Assert (To_String (Stanza.Stanza) = "this step works",
              "Text of the first given of background incorrect");

      Assert (Integer (Length (Feature.Scenarios)) /= 0,
              "No scenario");

      Assert (Integer (Length (Feature.Scenarios)) = 1,
              "More than one scenario");

      Scenario := Feature.Scenarios.Element (0);

      Assert (To_String (Scenario.Name) = "Run a good step",
              "Name of the scenario incorrect");

      Assert (Integer (Length (Scenario.Stanzas)) = 1,
              "No or more than one stanza in the scenario");

      Stanza := Scenario.Stanzas.Element (0);

      Assert (Stanza.Prefix = Prefix_Given,
              "The first step of the scenario is not a Given");

      Assert (To_String (Stanza.Stanza) = "this step works",
              "Text of the first given of scenario incorrect");

      Assert (To_String (Feature) = Canonical_Feature_Text,
              "To_String do not match the canonical text");

   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Reraise_Occurrence (Error);

   end Run_Test;

   --  Test_2  ----------------------------------------------------------------

   function  Name (T : in Test_2) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("AdaSpec.Features.Same simplest2.feature");
   end Name;

   procedure Run_Test (T : in out Test_2) is
      pragma Unreferenced (T);
      use Ada.Text_IO;

      Feature1 : Feature_Type := Null_Feature;
      Feature2 : Feature_File_Type;
      File     : constant String := "tests/features/simplest2.feature";
      Scenario : Scenario_Type;

   begin

      Make (Feature1, "Sample2");

      Assert (Name   (Feature1) = "Sample2", "Incorrect feature name");
      Assert (Parsed (Feature1), "Feature_Type is always parsed");

      Make   (Scenario, "Run a good step");
      Append (Scenario, Stanza_Given ("this step works"));
      Append (Scenario, Stanza_Given ("I am in front of a cake machine"));
      Append (Scenario, Stanza_When  ("I insert money"));
      Append (Scenario, Stanza_When  ("I push the button"));
      Append (Scenario, Stanza_Then  ("I get a cake"));
      Append (Feature1, Scenario);

      Make   (Scenario, "Background");
      Append (Scenario, Stanza_Given ("this step works"));
      Set_Background (Feature1, Scenario);

      Make  (Feature2, File);
      Parse (Feature2);

      Put_Line ("Feature1:");
      Put_Line (To_String (Feature1));
      Put_Line ("Feature2:");
      Put_Line (To_String (Feature2));

      Assert (To_String (Feature1) = To_String (Feature_Type (Feature2)),
              "The two features text representation must be the same");

      Assert (Same (Feature1, Feature2), "The two features must be the same");

   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Reraise_Occurrence (Error);

   end Run_Test;


   --  Test_3  ----------------------------------------------------------------

   function  Name (T : in Test_3) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("AdaSpec.Features null test");
   end Name;

   procedure Run_Test (T : in out Test_3) is
      pragma Unreferenced (T);
      use Ada.Text_IO;
   begin

      null;

   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Reraise_Occurrence (Error);

   end Run_Test;

end Test_Suite.Features;

