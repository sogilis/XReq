--                         Copyright (C) 2010, Sogilis                       --

with GNAT.OS_Lib;
with AUnit;
with AUnit.Test_Suites;


package Test_Suite.Main is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite);

   --  Test type
   type Test_1 is
      new Test_Case_Type with null record;

   --  Operation on Test_1
   function  Name (T : in     Test_1) return String;
   procedure Run  (T : in out Test_1);

private

   procedure Spawn_Assert  (Argument_String : in String;
                            Expected_Result : in Boolean := True;
                            Executable_Name : in String := "adaspec");

   function Command_Path  (Executable_Name : in String := "adaspec")
                           return String;

   function Command_Line  (Executable_Path : in String;
                           Argument_String : in String)
                           return String;

   function Argument_List (Command_Line    : in String)
                           return GNAT.OS_Lib.Argument_List_Access;

end Test_Suite.Main;
