--                         Copyright (C) 2010, Sogilis                       --

with AUnit;
with AUnit.Test_Suites;
with AUnit.Simple_Test_Cases;

package Test_Suite.Stanzas is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite);

   --  Test type
   type Test_1 is
      new AUnit.Simple_Test_Cases.Test_Case with null record;

   --  Operation on Test_1
   function  Name     (T : in     Test_1) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_1);

end Test_Suite.Stanzas;

