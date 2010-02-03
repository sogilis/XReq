--                         Copyright (C) 2010, Sogilis                       --

with AUnit;
with AUnit.Simple_Test_Cases;

package Test_Suite.CLI is

   --  Test type
   type Test is new AUnit.Simple_Test_Cases.Test_Case with null record;

   --  Operation on Test
   function Name (T : Test) return AUnit.Message_String;
   procedure Run_Test (T : in out Test);

end Test_Suite.CLI;
