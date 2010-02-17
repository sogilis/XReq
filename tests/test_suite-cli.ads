--                         Copyright (C) 2010, Sogilis                       --

with AUnit;


package Test_Suite.CLI is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite);

   --  Test type
   type Test_Help is new Test_Case_Type with null record;

   --  Operation on Test_Help
   function  Name (T : in     Test_Help) return String;
   procedure Run  (T : in out Test_Help);

end Test_Suite.CLI;
