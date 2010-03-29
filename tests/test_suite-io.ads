--                         Copyright (C) 2010, Sogilis                       --

with AUnit;
with AUnit.Test_Suites;


package Test_Suite.IO is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite);

   --  Test type
   type Test_1       is new Test_Case_Type with null record;
   type Test_Spawn   is new Test_Case_Type with null record;
   type Test_Char_IO is new Test_Case_Type with null record;
   type Test_Get_Set is new Test_Case_Type with null record;
   type Test_Logger  is new Test_Case_Type with null record;
   type Test_GetEnv  is new Test_Case_Type with null record;

   --  Operation on Test_1
   function  Name (T : in     Test_1) return String;
   procedure Run  (T : in out Test_1);

   --  Operation on Test_Spawn
   function  Name (T : in     Test_Spawn) return String;
   procedure Run  (T : in out Test_Spawn);

   --  Operation on Test_Char_IO
   function  Name (T : in     Test_Char_IO) return String;
   procedure Run  (T : in out Test_Char_IO);

   --  Operation on Test_Get_Set
   function  Name (T : in     Test_Get_Set) return String;
   procedure Run  (T : in out Test_Get_Set);

   --  Operation on Test_Logger
   function  Name (T : in     Test_Logger) return String;
   procedure Run  (T : in out Test_Logger);

   --  Operation on Test_GetEnv
   function  Name (T : in     Test_GetEnv) return String;
   procedure Run  (T : in out Test_GetEnv);

end Test_Suite.IO;

