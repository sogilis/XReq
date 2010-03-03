--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.IO_Exceptions;
with Util.IO;

use Ada.Strings.Unbounded;
use Util.IO;

package body Test_Suite.IO is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_1);
      Ret.Add_Test (new Test_Spawn);
      Ret.Add_Test (new Test_Char_IO);
      Ret.Add_Test (new Test_Get_Set);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.IO");
   end Name;

   procedure Run (T : in out Test_1) is
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

      T.Assert (Read_Whole_File (File_Name, CRLF) = File_Cnt,
              "Content of the file " & File_Name &
              " incorrect (Read_Whole_File).");

      Open (File, In_File, File_Name);

      T.Assert (not End_Of_File (File),
              "Missing 1st line of " & File_Name);

      T.Assert (To_String (Get_Whole_Line (File)) = Line_1,
              "First line of " & File_Name & " incorrect (Get_Whole_Line)");

      T.Assert (not End_Of_File (File),
              "Missing 2nd line of " & File_Name);

      declare
         s : constant String := Get_Whole_Line (File);
      begin
         T.Assert (s = Line_2,
                 "Second line of " & File_Name &
                 " incorrect (Get_Whole_Line)");
      end;

      T.Assert (End_Of_File (File),
              "No 3rd line expected in " & File_Name);

      declare
         procedure P;
         procedure P is begin
            T.Assert (To_String (Get_Whole_Line (File)) =
                      Null_Unbounded_String,
                      "Get_Line shouldn't return a string when end of file");
         end P;
         procedure Assert_Exception_Raised is new Assert_Except (Test_1, P);
      begin
         Assert_Exception_Raised (T, "Get_Line should raise " &
                                  "Ada.IO_Exceptions.End_Error at EOF",
                                  Ada.IO_Exceptions.End_Error'Identity);
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
      Output  : Unbounded_String;
      Success : Boolean;
      Result  : Integer;
   begin

      Spawn ("true", "", Output, Success, Result);
      T.Assert (Success, "Failure running `true`");
      T.Assert (Result = 0, "`true` returned error status"& Result'Img);

      Spawn ("false", "", Output, Success, Result);
      T.Assert (Success, "Failure running `false`");
      T.Assert (Result = 1, "`false` returned error status"& Result'Img);

      Output := Null_Unbounded_String;
      Spawn ("pwd", "", Output, Success, Result, "/tmp");

      T.Assert (Success, "Failure running `pwd`");
      T.Assert (Result = 0, "`pwd` returned error status" & Result'Img);
      T.Assert (To_String (Output) = "/tmp" & ASCII.LF,
              "Command output for `pwd` should be ""/tmp\n"" " &
              "instead of """ & To_String (Output) & """");

      Output := Null_Unbounded_String;
      Spawn ("printf", "%s-%s a b", Output, Success, Result);

      T.Assert (Success, "Failure running `printf %s-%s a b`");
      T.Assert (Result = 0,
              "`printf %s-%s a b` returned error status"& Result'Img);
      T.Assert (To_String (Output) = "a-b",
              "Command output for `printf %s-%s a b` should be ""a-b"" " &
              "instead of """ & To_String (Output) & """");

      declare
         procedure P;
         procedure P is begin
            Spawn ("ThIs_CoMmAnD_dOeSn_T_eXiStS", "", Output, Success, Result);
         end P;
         procedure A is new Assert_Except (Test_Spawn, P);
      begin
         A (T, "Spawn should raise Ada.IO_Exceptions.Name_Error when spawn " &
               """ThIs_CoMmAnD_dOeSn_T_eXiStS"". If you have this command " &
               "on your system, remove it to pass the test.",
               Ada.IO_Exceptions.Name_Error'Identity);
      end;

   end Run;

   --  Test_Char_IO  ----------------------------------------------------------

   function  Name (T : in Test_Char_IO) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.IO.Char_IO");
   end Name;

   procedure Run (T : in out Test_Char_IO) is
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

      T.Assert (To_String (Buffer) = Content,
              "File content incorrect. Found:" & ASCII.LF &
              "<<<" & To_String (Buffer) & ">>>" & ASCII.LF &
              "Instead of:" & ASCII.LF &
              "<<<" & Content & ">>>");

      Buffer := Null_Unbounded_String;
      Read_Whole_File (File, Buffer);
      Close (File);

      T.Assert (To_String (Buffer) = Content,
              "File content incorrect the second time. Found:" & ASCII.LF &
              "<<<" & To_String (Buffer) & ">>>" & ASCII.LF &
              "Instead of:" & ASCII.LF &
              "<<<" & Content & ">>>");

      Create (File, Out_File, File_Name & "-");
      Write_Whole_File (File, Content);
      Buffer := Null_Unbounded_String;
      Read_Whole_File (File, Buffer);
      Close (File);

      T.Assert (To_String (Buffer) = Content,
              "File content incorrect after write. Found:" & ASCII.LF &
              "<<<" & To_String (Buffer) & ">>>" & ASCII.LF &
              "Instead of:" & ASCII.LF &
              "<<<" & Content & ">>>");

   end Run;

   --  Test_Get_Set  ----------------------------------------------------------

   function  Name (T : in Test_Get_Set) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.IO.Get/Set_File");
   end Name;

   procedure Run (T : in out Test_Get_Set) is
      use Char_IO;
      File_Name : constant String := Temp_Name;
      Content   : constant String := "ABC" & ASCII.CR & "DEF" & ASCII.LF &
                                     "GHI" & ASCII.CR & ASCII.LF;
      Content2  : constant String := "JKL" & ASCII.CR & ASCII.LF;
   begin

      Set_File (File_Name, Content);

      declare
         S : constant String := Get_File (File_Name);
      begin
         T.Assert (S = Content,
                 "Set_File + Get_File doesn't keep the data intact");
      end;

      Append_File (File_Name, Content2);

      declare
         S : constant String := Get_File (File_Name);
      begin
         T.Assert (S = Content & Content2,
                 "Append_File not OK. Found:" & ASCII.LF &
              "<<<" & S & ">>>" & ASCII.LF &
              "Instead of:" & ASCII.LF &
              "<<<" & Content & Content2 & ">>>");
      end;

   end Run;

end Test_Suite.IO;

