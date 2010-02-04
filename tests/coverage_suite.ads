--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with AUnit;
with AUnit.Test_Suites;
with AUnit.Simple_Test_Cases;

package Coverage_Suite is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private

   --  Test type
   type Test is new AUnit.Simple_Test_Cases.Test_Case with
      record
         File : Ada.Strings.Unbounded.Unbounded_String;
         Path : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   --  Operation on Test
   function Name (T : in Test) return AUnit.Message_String;
   procedure Run_Test (T : in out Test);

   type Gcov_Line_Type is (Gcov_Line_Error,
                           Gcov_Line_Alive,
                           Gcov_Line_Dead,
                           Gcov_Line_Blank,
                           Gcov_End_Of_File);

   procedure Read_Gcov_Line (File   : in out Ada.Text_IO.File_Type;
                             Status : out    Gcov_Line_Type);
   procedure Read_Gcov (Filename         : in  String;
                        Out_Line_Count   : out Natural;
                        Out_Line_Covered : out Natural;
                        Out_Error        : out Integer);

end Coverage_Suite;
