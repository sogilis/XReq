--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers;
with Ada.Directories;
with AUnit.Assertions;
with AdaSpec;
with AdaSpec.Stanzas;
with AdaSpec.Steps;
with AdaSpec.Steps.Ada;

use Ada.Containers;
use Ada.Directories;
use AUnit.Assertions;
use AdaSpec;
use AdaSpec.Stanzas;
use AdaSpec.Steps;
use AdaSpec.Steps.Ada;

package body Test_Suite.Steps.Ada is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_Sample1);
      Ret.Add_Test (new Test_Parse_Dir);
   end Add_Tests;

   --  Sample1  ---------------------------------------------------------------

   function  Name (T : in Test_Sample1) return String is
      pragma Unreferenced (T);
   begin
      return ("AsaSpec.Steps.Ada sample1.ads");
   end Name;

   procedure Run (T : in out Test_Sample1) is
      pragma Unreferenced (T);
      Step  : Ada_Step_File_Type;
      File  : constant String := "tests/features/step_definitions/sample1.ads";
      Given1 : constant String := "this step works";
      Given2 : constant String := "this step does not work";
   begin

      Make (Step, File);

      Assert (File_Name (Step) = File,
              "Step filename (" & File_Name (Step) & ") is incorrect");

      Assert (not Parsed (Step),
              "Step has been parsed without invoking Parse");

      declare
         Foo : Boolean;
         procedure P;
         procedure P is begin
            Foo := Contains (Step, Stanza_Given (Given1));
            Assert (Foo and not Foo, "Should never reach here");
         end P;
         procedure Assert_Exception_Raised is new Assert_Exception (P);
      begin
         Assert_Exception_Raised ("Unparsed_Step has not been raised in call" &
                                  " to Contains");
      end;

      Parse (Step);

      Assert (Parsed (Step),
              "Step has not been parsed after invoking Parse");

      Assert (Contains (Step, Stanza_Given (Given1)),
              "Step should contain """ & Given1 & """");

      Assert (Contains (Step, Stanza_When ("this step works too")),
              "Step should contains `When this step works too'");

      Assert (not Contains (Step, Stanza_Given (Given2)),
              "Step should not contain """ & Given2 & """");

      Assert (Find (Step, Stanza_Given (Given1)) = "Sample1.This_Step_Works",
              "`Given " & Given1 & "' should find `Sample1.This_Step_Works'");

      Assert (Find (Step, Stanza_Given (Given2)) = "",
              "`Given " & Given2 & "' should find '");

   end Run;

   --  Parse_Directory  -------------------------------------------------------

   function  Name (T : in Test_Parse_Dir) return String is
      pragma Unreferenced (T);
   begin
      return ("AsaSpec.Steps.Ada.Parse_Directory");
   end Name;

   procedure Run (T : in out Test_Parse_Dir) is
      pragma Unreferenced (T);
      use Step_Vectors;

      Directory : constant String := "tests/features/step_definitions";
      Steps     : Step_Vectors.Vector;
      Step      : Step_File_Ptr;
   begin

      Parse_Directory (Steps, Directory);

      Assert (Length (Steps) = 1,
              "Detected " & Length (Steps)'Img &
              " steps instead of 1");

      Step := Element (Steps, 0);

      Assert (Simple_Name (File_Name (Step.all)) = "sample1.ads",
              "Should have detected step sample1.ads instead of " &
              File_Name (Step.all));

      Assert (Parsed (Step.all), "Should have parsed the step definition");

      Assert (Contains (Step.all, Stanza_Given ("this step works")),
              "The step definition should contain `Given this step works'");

   end Run;


end Test_Suite.Steps.Ada;
