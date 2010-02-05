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

   function  Name (T : in Test_1) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Util.IO");
   end Name;

   procedure Run_Test (T : in out Test_1) is
      pragma Unreferenced (T);
      File : File_Type;
   begin

      Util.IO.BufferSize := 5;

      Open (File, In_File, "tests/test_data/file1.txt");

      Assert (not End_Of_File (File),
              "Missing 1st line of test_data/file1.txt");

      Assert (To_String (Get_Whole_Line (File)) = "First Line",
              "First line of test_data/file1.txt incorrect");

      Assert (not End_Of_File (File),
              "Missing 2nd line of test_data/file1.txt");

      declare
         s : constant String := Get_Whole_Line (File);
      begin
         Assert (s = "Second Line",
                 "First line of test_data/file1.txt incorrect");
      end;

      Assert (End_Of_File (File),
              "No 3rd line expected in test_data/file1.txt");

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

   end Run_Test;

end Test_Suite.IO;

