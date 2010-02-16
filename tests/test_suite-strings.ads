--                         Copyright (C) 2010, Sogilis                       --

with AUnit;
with AUnit.Test_Suites;
with AUnit.Simple_Test_Cases;

package Test_Suite.Strings is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite);

   --  Test type
   type Test_Starts_With is
      new AUnit.Simple_Test_Cases.Test_Case with null record;
   type Test_Find_Token is
      new AUnit.Simple_Test_Cases.Test_Case with null record;
   type Test_Trimed_Suffix is
      new AUnit.Simple_Test_Cases.Test_Case with null record;
   type Test_To_Identifier is
      new AUnit.Simple_Test_Cases.Test_Case with null record;

   --  Operation on Test_Starts_With
   function  Name     (T : in     Test_Starts_With)
                      return AUnit.Message_String;
   procedure Run_Test (T : in out Test_Starts_With);

   --  Operation on Test_Find_Token
   function  Name     (T : in     Test_Find_Token) return AUnit.Message_String;
   procedure Run_Test (T : in out Test_Find_Token);

   --  Operation on Test_Trimed_Suffix
   function  Name     (T : in     Test_Trimed_Suffix)
                      return AUnit.Message_String;
   procedure Run_Test (T : in out Test_Trimed_Suffix);

   --  Operation on Test_To_Identifier
   function  Name     (T : in     Test_To_Identifier)
                      return AUnit.Message_String;
   procedure Run_Test (T : in out Test_To_Identifier);

end Test_Suite.Strings;

