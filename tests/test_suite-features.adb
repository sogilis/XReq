--                         Copyright (C) 2010, Sogilis                       --

with AUnit.Assertions;
with AdaSpec.Features;

use AUnit.Assertions;
use AdaSpec.Features;

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
      return AUnit.Format ("AdaSpec.Features");
   end Name;

   procedure Run_Test (T : in out Test_1) is
      pragma Unreferenced (T);
      Feature : Feature_File_Type;
      File    : constant String := "tests/features/simplest.feature";
   begin

      Make (Feature, File);

      Assert (File_Name (Feature) = File,
              "Feature filename (" & File_Name (Feature) & ") is incorrect");

      Assert (not Parsed (Feature),
              "Feature has been parsed without invoking Parse");

--       declare
--          Foo : Boolean;
--          procedure P;
--          procedure P is begin
--             Foo := Contains (Step, Prefix_Given, Given1);
--             Assert (Foo and not Foo, "Should never reach here");
--          end P;
--          procedure Assert_Exception_Raised is new Assert_Exception (P);
--       begin
--          Assert_Exception_Raised ("Unparsed_Feature has not been raised " &
--                                   "in call to Contains");
--       end;

      Parse (Feature);

      Assert (Parsed (Feature),
              "Feature has not been parsed after invoking Parse");


   end Run_Test;

end Test_Suite.Features;

