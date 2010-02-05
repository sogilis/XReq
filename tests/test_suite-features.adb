--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with AUnit.Assertions;
with AdaSpec;
with AdaSpec.Features;
with Util.Strings;

use Ada.Strings.Unbounded;
use AUnit.Assertions;
use AdaSpec;
use AdaSpec.Features;
use AdaSpec.Features.Stanza_Container;
use AdaSpec.Features.Scenario_Container;
use Util.Strings.Vectors;

package body Test_Suite.Features is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_1);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("AdaSpec.Features simplest.feature");
   end Name;

   procedure Run_Test (T : in out Test_1) is
      pragma Unreferenced (T);
      Feature  : Feature_File_Type;
      File     : constant String := "tests/features/simplest.feature";
      Scenario : Scenario_Type;
      Stanza   : Stanza_Type;
   begin

      Make (Feature, File);

      Assert (File_Name (Feature) = File,
              "Feature filename (" & File_Name (Feature) & ") is incorrect");

      Assert (not Parsed (Feature),
              "Feature has been parsed without invoking Parse");

      Parse (Feature);

      Assert (Parsed (Feature),
              "Feature has not been parsed after invoking Parse");

      Assert (To_String (Feature.Name) = "Sample",
              "Feature name incorrect");

      Assert (Integer (Length (Feature.Description)) = 0,
              "Feature description while there is none");

      Assert (To_String (Feature.Background.Name) = "",
              "Background name while there is none");

      Assert (Integer (Length (Feature.Background.Stanzas)) = 1,
              "No or more than one line of background");

      Stanza := Feature.Background.Stanzas.Element (0);

      Assert (Stanza.Prefix = Prefix_Given,
              "The first step of the background is not a Given");

      Assert (To_String (Stanza.Stanza) = "this step works",
              "Text of the first given of background incorrect");

      Assert (Integer (Length (Feature.Scenarios)) = 1,
              "No or more than one scenario");

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

   end Run_Test;

end Test_Suite.Features;

