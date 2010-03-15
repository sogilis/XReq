--                         Copyright (C) 2010, Sogilis                       --

with AUnit;
with AUnit.Test_Suites;

package Test_Suite.Lib.Asserts is

   procedure Add_Tests (Ret : in AUnit.Test_Suites.Access_Test_Suite);

   --  Test type
   type Test_Assert is new Test_Case_Type with null record;

   --  Operation on Test_Assert
   function  Name (T : in     Test_Assert) return String;
   procedure Run  (T : in out Test_Assert);

end Test_Suite.Lib.Asserts;

