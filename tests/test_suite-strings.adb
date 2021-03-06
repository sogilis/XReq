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

with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Util.Strings;

use Ada.Strings.Unbounded;
use Ada.Text_IO;
use Util.Strings;

package body Test_Suite.Strings is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_Starts_With);
      Ret.Add_Test (new Test_Find_Token);
      Ret.Add_Test (new Test_Trimed_Suffix);
      Ret.Add_Test (new Test_To_Identifier);
      Ret.Add_Test (new Test_Buffer);
      Ret.Add_Test (new Test_Ada_string);
      Ret.Add_Test (new Test_C_String);
      Ret.Add_Test (new Test_Decode_Python);
      Ret.Add_Test (new Test_Decode_String);
      Ret.Add_Test (new Test_Relative_Path);
      Ret.Add_Test (new Test_Reverse_Path);
      Ret.Add_Test (new Test_Goto_Path);
      Ret.Add_Test (new Test_Replace);
      Ret.Add_Test (new Test_Package_File_Id);
   end Add_Tests;

   --  Test_Starts_With  ------------------------------------------------------

   function  Name (T : in Test_Starts_With) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.Strings.Test_Starts_With");
   end Name;

   procedure Run (T : in out Test_Starts_With) is
      Search : constant String := "abc 123 ABC 456";
   begin

      T.Assert (Starts_With (Search, "abc"),
              "Should start with 'abc'");

      T.Assert (Starts_With (Search, "abc 123"),
              "Should start with 'abc 123'");

      T.Assert (Starts_With (Search, " 123", 4),
              "Should start with ' 123' at position 4");

      T.Assert (Starts_With (Search, "123 ABC", 5),
              "Should start with '123 ABC' at position 5");

      T.Assert (not Starts_With (Search, "123"),
              "Should not start with '123'");

      T.Assert (not Starts_With (Search, "4567", 13),
              "Should not start with '4567' at position 13");

   end Run;

   --  Test_Find_Token  -------------------------------------------------------

   function  Name (T : in Test_Find_Token) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.Strings.Find_Token");
   end Name;

   procedure Run (T : in out Test_Find_Token) is
      Search : constant String := " @tk1 A @tk2 B @tk3 C ";
      Tokens : constant String_List := (To_Unbounded_String ("@tk3"),
                                        To_Unbounded_String ("@tk2"),
                                        To_Unbounded_String ("@tk1"));
      Index  : Natural;
      Token  : Natural;
   begin

      Find_Token ("", Tokens, Index, Token);

      T.Assert (Token = 0, "token found found");
      T.Assert (Index = 0, "index out of bounds");

      Find_Token (Search, Tokens, Index, Token);

      T.Assert (Token /= 0, "No token found");
      T.Assert (Index /= 0, "No index found");

      T.Assert (Token = 3,
              "@tk1 not found, found token #" & Natural'Image (Token) & " " &
              To_String (Tokens (Token)));

      T.Assert (Index = 6,
              "@tk1 not at the correct position. Found: " &
              Natural'Image (Index) & " instead of 6");

   end Run;

   --  Test_Trimed_Suffix  ----------------------------------------------------

   function  Name (T : in Test_Trimed_Suffix) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.Strings.Trimed_Suffix");
   end Name;

   procedure Run (T : in out Test_Trimed_Suffix) is

      function Call (Source : in String;
                     Start_Index : in Natural) return String;

      function Call (Source : in String;
                     Start_Index : in Natural) return String is
         Result1 : constant String := Trimed_Suffix (Source, Start_Index);
         Result2 : constant String := To_String (
                   Trimed_Suffix (To_Unbounded_String (Source), Start_Index));
      begin
         T.Assert (Result1 = Result2, "Trimed_Suffix is not the same for " &
                 "type String and Unbounded_String. '" & Result1 & "' /= '" &
                 Result2 & "'");
         return Result1;
      end Call;

   begin

      T.Assert (Call ("   ABC   DEF  ",  1) = "ABC   DEF  ", "Error1");
      T.Assert (Call ("   ABC   DEF  ",  4) = "ABC   DEF  ", "Error2");
      T.Assert (Call ("   ABC   DEF  ",  5) =  "BC   DEF  ", "Error3");
      T.Assert (Call ("   ABC   DEF  ",  7) =       "DEF  ", "Error4");
      T.Assert (Call ("   ABC   DEF  ", 13) =            "", "Error5");
      T.Assert (Call ("   ABC   DEF  ", 20) =            "", "Error6");
      T.Assert (Call ("",               22) =            "", "Error7");
      T.Assert (Call ("",                0) =            "", "Error8");

   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Reraise_Occurrence (Error);

   end Run;

   --  Test_To_Identifier  ----------------------------------------------------

   function  Name (T : in Test_To_Identifier) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.Strings.To_Identifier");
   end Name;

   procedure Run (T : in out Test_To_Identifier) is
   begin

      T.Assert (To_Identifier ("This is a title ") = "This_is_a_title",
              "Error1");

      T.Assert (To_Identifier ("999 title ") = "title",
              "Error2");

      T.Assert (To_Identifier ("Test 7") = "Test_7",
              "Error3");

      T.Assert (To_Identifier ("!  Test & ( 8 ") = "Test_8",
              "Error4");

      T.Assert (To_Identifier ("toto_") = "toto",
              "Error5");

   end Run;

   --  Test_Buffer  -----------------------------------------------------------

   function  Name (T : in Test_Buffer) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.Strings.Buffer_Type");
   end Name;

   procedure Run (T : in out Test_Buffer) is
      B : Buffer_Type;
   begin

      T.Assert (B.Value = "", "Buffer should be empty when initialized");
      B.Put_Line ("first line");
      T.Assert (B.Value = "first line" & B.CRLF, "Put_Line not OK");
      B.Indent (5);
      B.Put_Indent;
      B.Put ("toto");
      B.New_Line;
      T.Assert (B.Value = "first line" & B.CRLF & "     toto" & B.CRLF,
              "(Indent (5); Put_Indent; Put; New_Line) not OK");
      B.Clear;
      T.Assert (B.Value = "", "Clear not OK");
      B.UnIndent (2);
      B.Put_Line (To_Unbounded_String ("tata"));
      T.Assert (B.Value = "   tata" & B.CRLF,
                "(UnIndent (2); Put_Line) not OK");
      B.UnIndent (3);
      B.Put_Indent;
      B.Put (To_Unbounded_String ("titi"));
      T.Assert (B.Value = "   tata" & B.CRLF & "titi",
              "(UnIndent (3); Put) not OK");

   end Run;

   --  Test_C_String  ------------------------------------------------------

   function  Name (T : in Test_C_String) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.Strings.C_String");
   end Name;

   procedure Run (T : in out Test_C_String) is
      Test1 : constant String := "This ""is"" a test";
      Res1  : constant String := """This \""is\"" a test""";

      Test2 : constant String := "This ""is"" a test" & ASCII.LF;
      Res2  : constant String := """This \""is\"" a test\012""";

      Test3 : constant String := "This ""is"" a test" & ASCII.LF & "nl";
      Res3  : constant String := """This \""is\"" a test\012nl""";
   begin

      T.Assert (C_String (Test1) = Res1,
              "Failed: " & Res1 & ASCII.LF &
              "Got: " & C_String (Test1));

      T.Assert (C_String (Test2) = Res2,
              "Failed: " & Res2 & ASCII.LF &
              "Got: " & C_String (Test2));

      T.Assert (C_String (Test3) = Res3,
              "Failed: " & Res3 & ASCII.LF &
              "Got: " & C_String (Test3));

   end Run;

   --  Test_Ada_string  ------------------------------------------------------

   function  Name (T : in Test_Ada_string) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.Strings.Ada_string");
   end Name;

   procedure Run (T : in out Test_Ada_string) is
      Test1 : constant String := "This ""is"" a test";
      Res1  : constant String := """This """"is"""" a test""";
      Test2 : constant String := "This ""is"" a test" & ASCII.LF;
      Res2  : constant String := """This """"is"""" a test"" & " &
                                 "Character'Val (10)";
      Test3 : constant String := "This ""is"" a test" & ASCII.LF & "nl";
      Res3  : constant String := """This """"is"""" a test"" & " &
                                 "Character'Val (10) & ""nl""";
   begin

      T.Assert (Ada_String (Test1) = Res1,
              "Failed: " & Res1 & ASCII.LF &
              "Got: " & Ada_String (Test1));

      T.Assert (Ada_String (Test2) = Res2,
              "Failed: " & Res2 & ASCII.LF &
              "Got: " & Ada_String (Test2));

      T.Assert (Ada_String (Test3) = Res3,
              "Failed: " & Res3 & ASCII.LF &
              "Got: " & Ada_String (Test3));

   end Run;

   --  Test_Decode_Python  ----------------------------------------------------

   function  Name (T : in Test_Decode_Python) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.Strings.Decode_Python");
   end Name;

   procedure Run (T : in out Test_Decode_Python) is

      procedure Compare (Python : in String; Original : in String);
      procedure Compare (Python : in String; Original : in String) is
         Str : constant String := Decode_Python (Python);
      begin
         T.Assert (Str = Original, "Decode_Python error." & ASCII.LF &
                 "Expected       """ & Original & """" & ASCII.LF &
                 "Got            """ & Str & """" & ASCII.LF &
                 "While decoding """ & Python & """");
      end Compare;
   begin

      Compare ("abc\" & ASCII.LF & "\\d\'e\""f", "abc\d'e""f");
      Compare ("a\ab\bf\fn\nn",     "a" & ASCII.BEL & "b" & ASCII.BS  &
                                    "f" & ASCII.FF  & "n" & ASCII.LF  & "n");
      Compare ("r\rt\tv\vv\222end", "r" & ASCII.CR  & "t" & ASCII.HT &
                                    "v" & ASCII.VT  & "v" &
                                    Character'Val (8#222#) & "end");
      Compare ("a\x56a\xfaa", "a" & Character'Val (16#56#) &
                              "a" & Character'Val (16#FA#) & "a");
      T.Assert (String'(Decode_Python ("h\hh", True)) = "h\hh",
              "Liberal not OK");
      declare
         procedure P;
         procedure P is begin
            T.Assert (String'(Decode_Python ("h\hh")) = "h\hh", "...");
         end P;
         procedure A is new Assert_Except (Test_Decode_Python, P);
      begin
         A (T, "Decode_Python should raise Constraint_Error",
            Constraint_Error'Identity);
      end;

   end Run;

   --  Test_Decode_String  ----------------------------------------------------

   function  Name (T : in Test_Decode_String) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.Strings.Decode_String");
   end Name;

   procedure Run (T : in out Test_Decode_String) is

      procedure Compare (Python : in String; Original : in String);
      procedure Compare (Python : in String; Original : in String) is
         Str : constant String := Decode_String (Python);
      begin
         T.Assert (Str = Original, "Decode_String error." & ASCII.LF &
                 "Expected       " & Ada_String (Original) & ASCII.LF &
                 "Got            " & Ada_String (Str) & ASCII.LF &
                 "While decoding """ & Python & """");
      end Compare;
   begin

      Compare ("abc\ndef", "abc" & ASCII.LF & "def");
      Compare ("abc\def",  "abc\def");
      Compare ("abcdef\",  "abcdef\");

   end Run;

   --  Test_Relative_Path  ----------------------------------------------------

   function  Name (T : in Test_Relative_Path) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.Strings.Relative_Path");
   end Name;

   procedure Run (T : in out Test_Relative_Path) is

      procedure Check (Prefix, Element, Result : in String);
      procedure Check (Prefix, Element, Result : in String) is
         Res : constant String := Relative_Path (Prefix, Element);
      begin
         T.Assert (Result = Res,
                   "Relative_Path (""" & Prefix & """, """ & Element & """) " &
                   "= """ & Res & """ /= """ & Result & """");
      end Check;

   begin

      Check ("toto/tata",       "titi",                 "toto/tata/titi");
      Check ("toto/tata",       "ti/ta",                "toto/tata/ti/ta");
      Check ("toto/tata",       "/ti/ta",               "/ti/ta");
      Check ("toto/tata/",      "titi",                 "toto/tata/titi");
      Check ("toto/tata/",      "ti/ta/",               "toto/tata/ti/ta/");
      Check ("toto/tata/",      "/ti/ta",               "/ti/ta");
      Check ("toto/tata",       "./a/ti",               "toto/tata/a/ti");
      Check ("toto/tata/",      "./a/ti/",              "toto/tata/a/ti/");
      Check ("toto/tata",       "../a/ti",              "toto/a/ti");
      Check ("toto/tata/",      "../a/ti",              "toto/a/ti");
      Check ("toto/tata/",      "./../../a/ti",         "a/ti");
      Check ("toto/",           "./../../a/ti",         "../a/ti");

   end Run;

   --  Test_Reverse_Path  -----------------------------------------------------

   function  Name (T : in Test_Reverse_Path) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.Strings.Reverse_Path");
   end Name;

   procedure Run (T : in out Test_Reverse_Path) is

      procedure Check (Path, Result : in String);
      procedure Check (Path, Result : in String) is
         Res : constant String := Reverse_Path (Path);
      begin
         T.Assert (Result = Res,
                   "Reverse_Path (""" & Path & """) " &
                   "= """ & Res & """ /= """ & Result & """");
      end Check;

   begin

      Check ("toto/tata",       "../..");
      Check ("toto/tata/",      "../..");
      Check ("toto",            "..");
      Check ("/toto",           "/");

   end Run;

   --  Test_Goto_Path  --------------------------------------------------------

   function  Name (T : in Test_Goto_Path) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.Strings.Goto_Path");
   end Name;

   procedure Run (T : in out Test_Goto_Path) is

      procedure Check (P1, P2, Result : in String);
      procedure Check (P1, P2, Result : in String) is
         Res : constant String := Goto_Path (P1, P2);
      begin
         T.Assert (Result = Res,
                   "Reverse_Path (""" & P1 & """, """ & P2 & """) " &
                   "= """ & Res & """ /= """ & Result & """");
      end Check;

   begin

      Check ("toto/tata",       "toto/titi",            "../titi");
      Check ("../toto/tata/",   "../tutu",              "../../tutu");
      Check ("toto/tata",       "toto/tata/titi",       "titi");
      Check ("toto/tata",       "toto/tatatiti",        "../tatatiti");
      Check ("toto/tata",       "toto",                 "..");
      Check ("toto/tata",       "tototata",             "../../tototata");
      Check ("feat/tests",      "feat/tests/a",         "a");
      Check ("feat/tests",      "feat/a.f",             "../a.f");
      Check ("feat/tests",      "feat/",                "..");
      Check ("feat/tests",      "feat",                 "..");

   end Run;

   --  Test_Replace  ----------------------------------------------------------

   function  Name (T : in Test_Replace) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.Strings.Replace");
   end Name;

   procedure Run (T : in out Test_Replace) is
   begin

      T.Assert (Replace ("abcabc", "abc", "Aabc") = "AabcAabc", "not OK");

   end Run;

   --  Test_Package_File_Id  --------------------------------------------------

   function  Name (T : in Test_Package_File_Id) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.Strings.To_Package_File_Id");
   end Name;

   procedure Run (T : in out Test_Package_File_Id) is
   begin

      T.Assert (To_Package_File_Id ("First_Second.Last") = "first_second-last",
                "To_Package_File_Id not OK");

   end Run;


end Test_Suite.Strings;
