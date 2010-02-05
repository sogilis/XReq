--                         Copyright (C) 2010, Sogilis                       --

with AUnit.Assertions;
with AdaSpec;
with AdaSpec.Steps;
with AdaSpec.Steps.Ada;

use AUnit.Assertions;
use AdaSpec;
use AdaSpec.Steps;
use AdaSpec.Steps.Ada;

package body Test_Suite.Steps.Ada is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_Sample1);
   end Add_Tests;

   --  Sample1  ---------------------------------------------------------------

   function  Name (T : in Test_Sample1) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("AsaSpec.Steps sample1.ads");
   end Name;

   procedure Run_Test (T : in out Test_Sample1) is
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
            Foo := Contains (Step, Prefix_Given, Given1);
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

      Assert (Contains (Step, Prefix_Given, Given1),
              "Step should contain """ & Given1 & """");

      Assert (not Contains (Step, Prefix_Given, Given2),
              "Step should not contain """ & Given2 & """");

   end Run_Test;


end Test_Suite.Steps.Ada;
