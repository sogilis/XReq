--                         Copyright (C) 2010, Sogilis                       --

with AUnit;
with AUnit.Test_Suites;


package Test_Suite.Result is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite);

   --  Test type
   type Test_Result_Step_Type is
      new Test_Case_Type with null record;
   type Test_Result_Scenario_Type is
      new Test_Case_Type with null record;
   type Test_Result_Feature_Type is
      new Test_Case_Type with null record;
   type Test_To_String is
      new Test_Case_Type with null record;

   --  Operation on Test_Result_Step_Type
   function  Name (T : in     Test_Result_Step_Type)
                           return String;
   procedure Run  (T : in out Test_Result_Step_Type);

   --  Operation on Test_Result_Scenario_Type
   function  Name (T : in     Test_Result_Scenario_Type)
                           return String;
   procedure Run  (T : in out Test_Result_Scenario_Type);

   --  Operation on Test_Result_Feature_Type
   function  Name (T : in     Test_Result_Feature_Type)
                           return String;
   procedure Run  (T : in out Test_Result_Feature_Type);

   --  Operation on Test_To_String
   function  Name (T : in     Test_To_String)
                           return String;
   procedure Run  (T : in out Test_To_String);

end Test_Suite.Result;

