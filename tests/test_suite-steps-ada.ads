--                         Copyright (C) 2010, Sogilis                       --

with AUnit;
with AUnit.Test_Suites;
with AUnit.Simple_Test_Cases;

package Test_Suite.Steps.Ada is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite);

   --  Test type
   type Test_Sample1 is
      new AUnit.Simple_Test_Cases.Test_Case with null record;
   type Test_Parse_Dir is
      new AUnit.Simple_Test_Cases.Test_Case with null record;

   --  Operation on Test_Sample1
   function  Name     (T : in     Test_Sample1) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_Sample1);

   --  Operation on Test_Parse_Dir
   function  Name     (T : in     Test_Parse_Dir) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_Parse_Dir);

end Test_Suite.Steps.Ada;
