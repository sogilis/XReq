--                         Copyright (C) 2010, Sogilis                       --

with AUnit;
with AUnit.Test_Suites;


package Test_Suite.Strings is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite);

   --  Test type
   type Test_Starts_With is new Test_Case_Type with null record;
   type Test_Find_Token is new Test_Case_Type with null record;
   type Test_Trimed_Suffix is new Test_Case_Type with null record;
   type Test_To_Identifier is new Test_Case_Type with null record;
   type Test_Buffer is new Test_Case_Type with null record;
   type Test_Ada_string is new Test_Case_Type with null record;
   type Test_Decode_Python is new Test_Case_Type with null record;

   --  Operation on Test_Starts_With
   function  Name (T : in     Test_Starts_With) return String;
   procedure Run  (T : in out Test_Starts_With);

   --  Operation on Test_Find_Token
   function  Name (T : in     Test_Find_Token) return String;
   procedure Run  (T : in out Test_Find_Token);

   --  Operation on Test_Trimed_Suffix
   function  Name (T : in     Test_Trimed_Suffix) return String;
   procedure Run  (T : in out Test_Trimed_Suffix);

   --  Operation on Test_To_Identifier
   function  Name (T : in     Test_To_Identifier) return String;
   procedure Run  (T : in out Test_To_Identifier);

   --  Operation on Test_Buffer
   function  Name (T : in     Test_Buffer) return String;
   procedure Run  (T : in out Test_Buffer);

   --  Operation on Test_Ada_string
   function  Name (T : in     Test_Ada_string) return String;
   procedure Run  (T : in out Test_Ada_string);

   --  Operation on Test_Decode_Python
   function  Name (T : in     Test_Decode_Python) return String;
   procedure Run  (T : in out Test_Decode_Python);

end Test_Suite.Strings;

