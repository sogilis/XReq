--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with AUnit.Assertions;
with Util.IO;

use Ada.Text_IO;
use Ada.Strings.Unbounded;
use AUnit.Assertions;
use Util.IO;

package body Test_Suite.IO is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_1);
   end Add_Tests;

   --  Test_1  ----------------------------------------------------------------

   function  Name (T : in Test_1) return String is
      pragma Unreferenced (T);
   begin
      return ("Util.IO");
   end Name;

   procedure Run (T : in out Test_1) is
      pragma Unreferenced (T);
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

end Test_Suite.IO;

