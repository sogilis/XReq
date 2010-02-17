--                         Copyright (C) 2010, Sogilis                       --

with AUnit;
with AUnit.Test_Suites;


package Test_Suite.Steps.Ada is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite);

   --  Test type
   type Test_Sample1 is
      new Test_Case_Type with null record;
   type Test_Parse_Dir is
      new Test_Case_Type with null record;

   --  Operation on Test_Sample1
   function  Name (T : in     Test_Sample1) return String;
   procedure Run  (T : in out Test_Sample1);

   --  Operation on Test_Parse_Dir
   function  Name (T : in     Test_Parse_Dir) return String;
   procedure Run  (T : in out Test_Parse_Dir);

end Test_Suite.Steps.Ada;
