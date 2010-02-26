--                         Copyright (C) 2010, Sogilis                       --

with AUnit;
with AUnit.Test_Suites;


package Test_Suite.Main is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite);

   --  Test type
   type Test_1 is new Test_Case_Type with null record;
   type Test_2 is new Test_Case_Type with null record;

   --  Operation on Test_1
   function  Name (T : in     Test_1) return String;
   procedure Run  (T : in out Test_1);

   --  Operation on Test_2
   function  Name (T : in     Test_2) return String;
   procedure Run  (T : in out Test_2);

private

   generic
      type Test_Case_Generic_Type (<>) is
         abstract new Test_Case_Type with private;
   procedure Spawn_Assert  (T               : in Test_Case_Generic_Type;
                            Argument_String : in String;
                            Expected_Result : in Boolean := True;
                            Directory       : in String := "";
                            Executable_Name : in String := "bin/adaspec");

end Test_Suite.Main;
