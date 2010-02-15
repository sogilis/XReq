--                         Copyright (C) 2010, Sogilis                       --

with AUnit;
with AUnit.Test_Suites;
with AUnit.Simple_Test_Cases;

package Test_Suite.Job is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite);

   --  Test type
   type Test_Describe is
      new AUnit.Simple_Test_Cases.Test_Case with null record;
   type Test_Fill_Missing is
      new AUnit.Simple_Test_Cases.Test_Case with null record;
   type Test_Fill_Missing_2 is
      new AUnit.Simple_Test_Cases.Test_Case with null record;
   type Test_Run is
      new AUnit.Simple_Test_Cases.Test_Case with null record;

   --  Operation on Test_Describe
   function  Name     (T : in     Test_Describe) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_Describe);

   --  Operation on Test_Fill_Missing
   function  Name     (T : in     Test_Fill_Missing)
      return AUnit.Message_String;
   procedure Run_Test (T : in out Test_Fill_Missing);

   --  Operation on Test_Fill_Missing_2
   function  Name     (T : in     Test_Fill_Missing_2)
      return AUnit.Message_String;
   procedure Run_Test (T : in out Test_Fill_Missing_2);

   --  Operation on Test_Run
   function  Name     (T : in     Test_Run) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_Run);

end Test_Suite.Job;
