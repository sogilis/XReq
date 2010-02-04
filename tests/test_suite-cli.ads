--                         Copyright (C) 2010, Sogilis                       --

with AUnit;
with AUnit.Simple_Test_Cases;

package Test_Suite.CLI is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite);

   --  Test type
   type Test_Help is new AUnit.Simple_Test_Cases.Test_Case with null record;

   --  Operation on Test_Help
   function  Name     (T : in     Test_Help) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_Help);

end Test_Suite.CLI;
