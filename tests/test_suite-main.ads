--                         Copyright (C) 2010, Sogilis                       --

with GNAT.OS_Lib;
with AUnit;
with AUnit.Test_Suites;
with AUnit.Simple_Test_Cases;

package Test_Suite.Main is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite);

   --  Test type
   type Test_1 is
      new AUnit.Simple_Test_Cases.Test_Case with null record;

   --  Operation on Test_1
   function  Name     (T : in     Test_1) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_1);

private

   function Command_Path  (Executable_Name : in String := "adaspec")
                           return String;

   function Command_Line  (Executable_Path : in String;
                           Argument_String : in String)
                           return String;

   function Argument_List (Executable_Path : in String;
                           Argument_String : in String)
                           return GNAT.OS_Lib.Argument_List_Access;

   function Argument_List (Command_Line    : in String)
                           return GNAT.OS_Lib.Argument_List_Access;

end Test_Suite.Main;
