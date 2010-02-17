--                         Copyright (C) 2010, Sogilis                       --

with AUnit;
with AUnit.Test_Suites;


package Test_Suite.Job is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite);

   --  Test type
   type Test_Describe is
      new Test_Case_Type with null record;
   type Test_Fill_Missing is
      new Test_Case_Type with null record;
   type Test_Job_Environment is
      new Test_Case_Type with null record;
   type Test_Run  is
      new Test_Case_Type with null record;

   --  Operation on Test_Describe
   function  Name (T : in     Test_Describe) return String;
   procedure Run  (T : in out Test_Describe);

   --  Operation on Test_Fill_Missing
   function  Name (T : in     Test_Fill_Missing)
      return String;
   procedure Run  (T : in out Test_Fill_Missing);

   --  Operation on Test_Job_Environment
   function  Name (T : in     Test_Job_Environment)
      return String;
   procedure Run  (T : in out Test_Job_Environment);

   --  Operation on Test_Run
   function  Name (T : in     Test_Run) return String;
   procedure Run  (T : in out Test_Run);

end Test_Suite.Job;
