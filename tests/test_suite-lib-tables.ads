--                         Copyright (C) 2010, Sogilis                       --

with AUnit;
with AUnit.Test_Suites;

package Test_Suite.Lib.Tables is

   procedure Add_Tests (Ret : in AUnit.Test_Suites.Access_Test_Suite);

   --  Test type
   type Test_1 is new Test_Case_Type with null record;
   type Test_2 is new Test_Case_Type with null record;

   --  Operation on Test_1
   function  Name (T : in     Test_1) return String;
   procedure Run  (T : in out Test_1);

   --  Operation on Test_2
   function  Name (T : in     Test_2) return String;
   procedure Run  (T : in out Test_2);

end Test_Suite.Lib.Tables;

