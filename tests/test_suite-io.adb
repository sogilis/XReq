--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with AUnit.Assertions;
with Util.IO;

use Ada.Strings.Unbounded;
use AUnit.Assertions;
use Util.IO;

package body Test_Suite.IO is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_1);
      Ret.Add_Test (new Test_Spawn);
      Ret.Add_Test (new Test_Char_IO);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.IO");
   end Name;

   procedure Run (T : in out Test_1) is
      pragma Unreferenced (T);
      use Ada.Text_IO;
      File_Name : constant String := "tests/test_data/file1.txt";
      CRLF      : constant String := "" & ASCII.LF;
      Line_1    : constant String := "First Line";
      Line_2    : constant String := "Second Line";
      File_Cnt  : constant String
                := Line_1 & CRLF & Line_2 & CRLF;
      File      : File_Type;
   begin

      Util.IO.BufferSize := 5;

      Assert (Read_Whole_File (File_Name, CRLF) = File_Cnt,
              "Content of the file " & File_Name &
              " incorrect (Read_Whole_File).");

      Open (File, In_File, File_Name);

      Assert (not End_Of_File (File),
              "Missing 1st line of " & File_Name);

      Assert (To_String (Get_Whole_Line (File)) = Line_1,
              "First line of " & File_Name & " incorrect (Get_Whole_Line)");

      Assert (not End_Of_File (File),
              "Missing 2nd line of " & File_Name);

      declare
         s : constant String := Get_Whole_Line (File);
      begin
         Assert (s = Line_2,
                 "Second line of " & File_Name &
                 " incorrect (Get_Whole_Line)");
      end;

      Assert (End_Of_File (File),
              "No 3rd line expected in " & File_Name);

      declare
         procedure P;
         procedure P is begin
            Assert (To_String (Get_Whole_Line (File)) = Null_Unbounded_String,
                  "Get_Line shouldn't return a string when end of file");
         end P;
         procedure Assert_Exception_Raised is new Assert_Exception (P);
      begin
         Assert_Exception_Raised ("Get_Line should raise " &
                                  "Ada.IO_Exceptions.End_Error at EOF");
      end;

      Close (File);

   end Run;

   --  Test_Spawn  ------------------------------------------------------------

   function  Name (T : in Test_Spawn) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.IO.Spawn");
   end Name;

   procedure Run (T : in out Test_Spawn) is
      pragma Unreferenced (T);
      Output  : Unbounded_String;
      Success : Boolean;
      Result  : Integer;
   begin

      Spawn ("true", "", Output, Success, Result);
      Assert (Success, "Failure running `true`");
      Assert (Result = 0, "`true` returned error status"& Result'Img);

      Spawn ("false", "", Output, Success, Result);
      Assert (Success, "Failure running `false`");
      Assert (Result = 1, "`false` returned error status"& Result'Img);

      Output := Null_Unbounded_String;
      Spawn ("pwd", "", Output, Success, Result, "/tmp");

      Assert (Success, "Failure running `pwd`");
      Assert (Result = 0, "`pwd` returned error status" & Result'Img);
      Assert (To_String (Output) = "/tmp" & ASCII.LF,
              "Command output for `pwd` should be ""/tmp\n"" " &
              "instead of """ & To_String (Output) & """");

      Output := Null_Unbounded_String;
      Spawn ("printf", "%s-%s a b", Output, Success, Result);

      Assert (Success, "Failure running `printf %s-%s a b`");
      Assert (Result = 0,
              "`printf %s-%s a b` returned error status"& Result'Img);
      Assert (To_String (Output) = "a-b",
              "Command output for `printf %s-%s a b` should be ""a-b"" " &
              "instead of """ & To_String (Output) & """");

   end Run;

   --  Test_Char_IO  ----------------------------------------------------------

   function  Name (T : in Test_Char_IO) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.IO.Char_IO");
   end Name;

   procedure Run (T : in out Test_Char_IO) is
      pragma Unreferenced (T);
      use Char_IO;
      File_Name : constant String := "tests/test_data/file1.txt";
      Content   : constant String
                := "First Line" & ASCII.LF & "Second Line";
      File      : Char_IO.File_Type;
      Buffer    : Unbounded_String;
   begin

      Open (File, Append_File, File_Name);
      Buffer := Null_Unbounded_String;
      Read_Whole_File (File, Buffer);

      Assert (To_String (Buffer) = Content,
              "File content incorrect. Found:" & ASCII.LF &
              "<<<" & To_String (Buffer) & ">>>" & ASCII.LF &
              "Instead of:" & ASCII.LF &
              "<<<" & Content & ">>>");

      Buffer := Null_Unbounded_String;
      Read_Whole_File (File, Buffer);
      Close (File);

      Assert (To_String (Buffer) = Content,
              "File content incorrect the second time. Found:" & ASCII.LF &
              "<<<" & To_String (Buffer) & ">>>" & ASCII.LF &
              "Instead of:" & ASCII.LF &
              "<<<" & Content & ">>>");

   end Run;

end Test_Suite.IO;

