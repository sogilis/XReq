--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with AUnit;
with AUnit.Test_Suites;
with AUnit.Simple_Test_Cases;
with Ada.Containers.Ordered_Maps;

package Coverage_Suite is

   function Suite return AUnit.Test_Suites.Access_Test_Suite;

private

   package LCov_Matches_Maps is new
      Ada.Containers.Ordered_Maps (Natural, Boolean, "<", "=");

   type LCov_Test is new AUnit.Simple_Test_Cases.Test_Case with
      record
         File_Name : Ada.Strings.Unbounded.Unbounded_String;
         Lines     : LCov_Matches_Maps.Map;
      end record;
   type LCov_Test_Ptr is access all LCov_Test;

   function Name (T : in LCov_Test) return AUnit.Message_String;
   procedure Run_Test (T : in out LCov_Test);

   --  Test type
   type Test is new AUnit.Simple_Test_Cases.Test_Case with
      record
         File : Ada.Strings.Unbounded.Unbounded_String;
         Path : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   --  Operation on Test
   function Name (T : in Test) return AUnit.Message_String;
   procedure Run_Test (T : in out Test);

   function PkgName (T : in Test) return String;

   type Gcov_Line_Type is (Gcov_Line_Error,
                           Gcov_Line_Alive,
                           Gcov_Line_Dead,
                           Gcov_Line_Blank,
                           Gcov_Line_Ignored,
                           Gcov_End_Of_File);

   procedure Read_Gcov_Line (File   : in out Ada.Text_IO.File_Type;
                             Status : out    Gcov_Line_Type;
                             Ignore : in out Boolean);
   procedure Read_Gcov (Filename         : in  String;
                        Out_Line_Count   : out Natural;
                        Out_Line_Covered : out Natural;
                        Out_Line_Ignored : out Natural;
                        Out_Error        : out Integer);

end Coverage_Suite;
