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
