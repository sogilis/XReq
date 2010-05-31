-------------------------------------------------------------------------------
--  XReq  --  Behaviour Driven Developpement tool for compiled languages     --
--  Copyright (c) 2010, SOGILIS <http://sogilis.com>                         --
--                                                                           --
--  This program is free software: you can redistribute it and/or modify     --
--  it under the terms of the GNU Affero General Public License as           --
--  published by the Free Software Foundation, either version 3 of the       --
--  License, or (at your option) any later version.                          --
--                                                                           --
--  This program is distributed in the hope that it will be useful,          --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of           --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            --
--  GNU Affero General Public License for more details.                      --
--                                                                           --
--  You should have received a copy of the GNU Affero General Public License --
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.    --
--                                                                           --
-------------------------------------------------------------------------------

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
   type Test_C_String is new Test_Case_Type with null record;
   type Test_Decode_Python is new Test_Case_Type with null record;
   type Test_Decode_String is new Test_Case_Type with null record;
   type Test_Relative_Path is new Test_Case_Type with null record;
   type Test_Reverse_Path is new Test_Case_Type with null record;
   type Test_Goto_Path is new Test_Case_Type with null record;
   type Test_Replace is new Test_Case_Type with null record;
   type Test_Package_File_Id is new Test_Case_Type with null record;

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

   --  Operation on Test_C_String
   function  Name (T : in     Test_C_String) return String;
   procedure Run  (T : in out Test_C_String);

   --  Operation on Test_Decode_Python
   function  Name (T : in     Test_Decode_Python) return String;
   procedure Run  (T : in out Test_Decode_Python);

   --  Operation on Test_Decode_String
   function  Name (T : in     Test_Decode_String) return String;
   procedure Run  (T : in out Test_Decode_String);

   --  Operation on Test_Relative_Path
   function  Name (T : in     Test_Relative_Path) return String;
   procedure Run  (T : in out Test_Relative_Path);

   --  Operation on Test_Reverse_Path
   function  Name (T : in     Test_Reverse_Path) return String;
   procedure Run  (T : in out Test_Reverse_Path);

   --  Operation on Test_Goto_Path
   function  Name (T : in     Test_Goto_Path) return String;
   procedure Run  (T : in out Test_Goto_Path);

   --  Operation on Test_Replace
   function  Name (T : in     Test_Replace) return String;
   procedure Run  (T : in out Test_Replace);

   --  Operation on Test_Package_File_Id
   function  Name (T : in     Test_Package_File_Id) return String;
   procedure Run  (T : in out Test_Package_File_Id);

end Test_Suite.Strings;
