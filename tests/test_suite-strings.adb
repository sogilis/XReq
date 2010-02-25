--                         Copyright (C) 2010, Sogilis                       --

with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with AUnit.Assertions;
with Util.Strings;

use Ada.Exceptions;
use Ada.Strings.Unbounded;
use Ada.Text_IO;
use AUnit.Assertions;
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
      Ret.Add_Test (new Test_Decode_Python);
   end Add_Tests;

   --  Test_Starts_With  ------------------------------------------------------

   function  Name (T : in Test_Starts_With) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.Strings.Test_Starts_With");
   end Name;

   procedure Run (T : in out Test_Starts_With) is
      pragma Unreferenced (T);
      Search : constant String := "abc 123 ABC 456";
   begin

      Assert (Starts_With (Search, "abc"),
              "Should start with 'abc'");

      Assert (Starts_With (Search, "abc 123"),
              "Should start with 'abc 123'");

      Assert (Starts_With (Search, " 123", 4),
              "Should start with ' 123' at position 4");

      Assert (Starts_With (Search, "123 ABC", 5),
              "Should start with '123 ABC' at position 5");

      Assert (not Starts_With (Search, "123"),
              "Should not start with '123'");

      Assert (not Starts_With (Search, "4567", 13),
              "Should not start with '4567' at position 13");

   end Run;

   --  Test_Find_Token  -------------------------------------------------------

   function  Name (T : in Test_Find_Token) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.Strings.Find_Token");
   end Name;

   procedure Run (T : in out Test_Find_Token) is
      pragma Unreferenced (T);
      Search : constant String := " @tk1 A @tk2 B @tk3 C ";
      Tokens : constant String_List := (To_Unbounded_String ("@tk3"),
                                        To_Unbounded_String ("@tk2"),
                                        To_Unbounded_String ("@tk1"));
      Index  : Natural;
      Token  : Natural;
   begin

      Find_Token ("", Tokens, Index, Token);

      Assert (Token = 0, "token found found");
      Assert (Index = 0, "index out of bounds");

      Find_Token (Search, Tokens, Index, Token);

      Assert (Token /= 0, "No token found");
      Assert (Index /= 0, "No index found");

      Assert (Token = 3,
              "@tk1 not found, found token #" & Natural'Image (Token) & " " &
              To_String (Tokens (Token)));

      Assert (Index = 6,
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
      pragma Unreferenced (T);

      function Call (Source : in String;
                     Start_Index : in Natural) return String;

      function Call (Source : in String;
                     Start_Index : in Natural) return String is
         Result1 : constant String := Trimed_Suffix (Source, Start_Index);
         Result2 : constant String := To_String (
                   Trimed_Suffix (To_Unbounded_String (Source), Start_Index));
      begin
         Assert (Result1 = Result2, "Trimed_Suffix is not the same for " &
                 "type String and Unbounded_String. '" & Result1 & "' /= '" &
                 Result2 & "'");
         return Result1;
      end Call;

   begin

      Assert (Call ("   ABC   DEF  ",  1) = "ABC   DEF  ", "Error1");
      Assert (Call ("   ABC   DEF  ",  4) = "ABC   DEF  ", "Error2");
      Assert (Call ("   ABC   DEF  ",  5) =  "BC   DEF  ", "Error3");
      Assert (Call ("   ABC   DEF  ",  7) =       "DEF  ", "Error4");
      Assert (Call ("   ABC   DEF  ", 13) =            "", "Error5");
      Assert (Call ("   ABC   DEF  ", 20) =            "", "Error6");
      Assert (Call ("",               22) =            "", "Error7");
      Assert (Call ("",                0) =            "", "Error8");

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
      pragma Unreferenced (T);
   begin

      Assert (To_Identifier ("This is a title ") = "This_is_a_title",
              "Error1");

      Assert (To_Identifier ("999 title ") = "title",
              "Error2");

      Assert (To_Identifier ("Test 7") = "Test_7",
              "Error3");

      Assert (To_Identifier ("!  Test & ( 8 ") = "Test_8",
              "Error4");

      Assert (To_Identifier ("toto_") = "toto",
              "Error5");

   end Run;

   --  Test_Buffer  -----------------------------------------------------------

   function  Name (T : in Test_Buffer) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.Strings.Buffer_Type");
   end Name;

   procedure Run (T : in out Test_Buffer) is
      pragma Unreferenced (T);
      B : Buffer_Type;
   begin

      Assert (B.Value = "", "Buffer should be empty when initialized");
      B.Put_Line ("first line");
      Assert (B.Value = "first line" & B.CRLF, "Put_Line not OK");
      B.Indent (5);
      B.Put_Indent;
      B.Put ("toto");
      B.New_Line;
      Assert (B.Value = "first line" & B.CRLF & "     toto" & B.CRLF,
              "(Indent (5); Put_Indent; Put; New_Line) not OK");
      B.Clear;
      Assert (B.Value = "", "Clear not OK");
      B.UnIndent (2);
      B.Put_Line (To_Unbounded_String ("tata"));
      Assert (B.Value = "   tata" & B.CRLF, "(UnIndent (2); Put_Line) not OK");
      B.UnIndent (3);
      B.Put_Indent;
      B.Put (To_Unbounded_String ("titi"));
      Assert (B.Value = "   tata" & B.CRLF & "titi",
              "(UnIndent (3); Put) not OK");

   end Run;

   --  Test_Ada_string  ------------------------------------------------------

   function  Name (T : in Test_Ada_string) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.Strings.Ada_string");
   end Name;

   procedure Run (T : in out Test_Ada_string) is
      pragma Unreferenced (T);
      Test1 : constant String := "This ""is"" a test";
      Res1  : constant String := """This """"is"""" a test""";
      Test2 : constant String := "This ""is"" a test" & ASCII.LF;
      Res2  : constant String := """This """"is"""" a test"" & " &
                                 "Character'Val (10)";
      Test3 : constant String := "This ""is"" a test" & ASCII.LF & "nl";
      Res3  : constant String := """This """"is"""" a test"" & " &
                                 "Character'Val (10) & ""nl""";
   begin

      Assert (Ada_String (Test1) = Res1,
              "Failed: " & Res1 & ASCII.LF &
              "Got: " & Ada_String (Test1));

      Assert (Ada_String (Test2) = Res2,
              "Failed: " & Res2 & ASCII.LF &
              "Got: " & Ada_String (Test2));

      Assert (Ada_String (Test3) = Res3,
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
      pragma Unreferenced (T);

      procedure Compare (Python : in String; Original : in String);
      procedure Compare (Python : in String; Original : in String) is
         Str : constant String := Decode_Python (Python);
      begin
         Assert (Str = Original, "Decode_Python error." & ASCII.LF &
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
      Assert (Decode_Python ("h\hh", True) = "h\hh", "Liberal not OK");
      declare
         procedure P;
         procedure P is begin
            Assert (Decode_Python ("h\hh") = "h\hh", "...");
         end P;
         procedure Assert_Exception_Raised is new Assert_Exception (P);
      begin
         Assert_Exception_Raised ("Decode_Python should raise " &
                                  "Constraint_Error");
      end;

   end Run;

end Test_Suite.Strings;

