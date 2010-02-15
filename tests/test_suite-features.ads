--                         Copyright (C) 2010, Sogilis                       --

with AUnit;
with AUnit.Test_Suites;
with AUnit.Simple_Test_Cases;

package Test_Suite.Features is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite);

   --  Test type
   type Test_1 is new AUnit.Simple_Test_Cases.Test_Case with null record;
   type Test_2 is new AUnit.Simple_Test_Cases.Test_Case with null record;
   type Test_3 is new AUnit.Simple_Test_Cases.Test_Case with null record;

   --  Operation on Test_1
   function  Name     (T : in     Test_1) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_1);

   --  Operation on Test_2
   function  Name     (T : in     Test_2) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_2);

   --  Operation on Test_3
   function  Name     (T : in     Test_3) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_3);

end Test_Suite.Features;

