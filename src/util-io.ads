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
with Ada.Sequential_IO;
with GNAT.OS_Lib;

use Ada.Strings.Unbounded;

package Util.IO is

   -----------------
   --  Temp_Name  --
   -----------------

   function Temp_Name return String;

   ---------------
   --  Char_IO  --
   ---------------

   package Char_IO is new Ada.Sequential_IO (Character);

   procedure Read_Whole_File  (File   : in out Char_IO.File_Type;
                               Buffer : in out Unbounded_String);

   procedure Write_Whole_File (File   : in out Char_IO.File_Type;
                               Buffer : in String);

   function  Get_File    (File_Name : in String) return String;
   procedure Set_File    (File_Name : in String;
                          Content   : in String);
   procedure Append_File (File_Name : in String;
                          Content   : in String);

   -----------------------------------
   --  Text_IO  --  Get_Whole_Line  --
   -----------------------------------

   BufferSize : Natural := 2000;

   --
   --  Ada.Text_IO.Get_Line seems to be able to get the complete line
   --
   function Get_Whole_Line  (File : in Ada.Text_IO.File_Type)
                                return Unbounded_String;
   function Get_Whole_Line  (File : in Ada.Text_IO.File_Type)
                                return String;

   ------------------------------------
   --  Text_IO  --  Read_Whole_File  --
   ------------------------------------

   function Read_Whole_File (File_Name : in String;
                             CRLF      : in String := "" & ASCII.LF)
                                         return String;

   -------------
   --  Spawn  --
   -------------

   procedure Spawn          (Command_Name  : in     String;
                             Args          : in     GNAT.OS_Lib.Argument_List;
                             Output_Buffer : in out Unbounded_String;
                             Success       : out    Boolean;
                             Return_Code   : out    Integer;
                             Err_To_Out    : in     Boolean := True);

   procedure Spawn          (Command_Name  : in     String;
                             Command_Line  : in     String;
                             Output_Buffer : in out Unbounded_String;
                             Success       : out    Boolean;
                             Return_Code   : out    Integer;
                             Directory     : in     String := "");

   procedure System         (Script        : in String;
                             Output_Buffer : in out Unbounded_String;
                             Return_Code   : out    Integer;
                             Err_To_Out    : in     Boolean := True);

   --------------
   --  GetEnv  --
   --------------

   function GetEnv (VarName, Default : in String) return String;

   -------------------
   --  Logger_Type  --
   -------------------

--    type Logger_Type is abstract tagged private;
   type Logger_Type is abstract tagged
      record
         Verbosity_Level : Integer := 0;
         Indent_Level    : Natural := 0;
      end record;
   type Logger_Ptr  is access all Logger_Type'Class;

   procedure Set_Verbosity (Log : in out Logger_Type;
                            V   : in     Integer);
   function  Verbosity     (Log : in     Logger_Type)
                                  return Integer;
   procedure Put_Line      (Log : in out Logger_Type;
                            S   : in     String;
                            V   : in     Integer := 0);
   procedure Put_Line      (Log : in out Logger_Type;
                            S   : in     Unbounded_String;
                            V   : in     Integer := 0);
   procedure Put           (Log : in out Logger_Type;
                            S   : in     String;
                            V   : in     Integer := 0);
   procedure Put           (Log : in out Logger_Type;
                            S   : in     Unbounded_String;
                            V   : in     Integer := 0);
   procedure Put_Line      (Log : in out Logger_Type;
                            V   : in     Integer;
                            S   : in     String);
   procedure Put_Line      (Log : in out Logger_Type;
                            V   : in     Integer;
                            S   : in     Unbounded_String);
   procedure Put           (Log : in out Logger_Type;
                            V   : in     Integer;
                            S   : in     String);
   procedure Put           (Log : in out Logger_Type;
                            V   : in     Integer;
                            S   : in     Unbounded_String);
   procedure Put_Indent    (Log : in out Logger_Type;
                            V   : in     Integer := 0);
   procedure New_Line      (Log : in out Logger_Type;
                            V   : in     Integer := 0);
   procedure Indent        (Log : in out Logger_Type;
                            N   : in     Natural := 3);
   procedure UnIndent      (Log : in out Logger_Type;
                            N   : in     Natural := 3);
   procedure Put_Always    (Log : in out Logger_Type;
                            S   : in     String) is abstract;

   procedure Free          (Log : in out Logger_Ptr);

   ------------------------
   --  Null_Logger_Type  --
   ------------------------

   type Null_Logger_Type is new Logger_Type with null record;
   type Null_Logger_Ptr is access all Null_Logger_Type'Class;

   function New_Null_Logger return Null_Logger_Ptr;

   procedure Put_Always    (Log : in out Null_Logger_Type;      --  GCOV_IGNORE
                            S   : in     String) is null;       --  GCOV_IGNORE

   Expanded_Null_Logger : aliased Null_Logger_Type;
   Null_Logger : Logger_Ptr := Expanded_Null_Logger'Access;

   ----------------------------
   --  Standard_Logger_Type  --
   ----------------------------

   type Standard_Logger_Type is new Logger_Type with null record;
   type Standard_Logger_Ptr is access all Standard_Logger_Type'Class;

   function New_Standard_Logger return Standard_Logger_Ptr;

   procedure Put_Always    (Log : in out Standard_Logger_Type;
                            S   : in     String);

   Expanded_Std_Logger : aliased Standard_Logger_Type;
   Std_Logger : Logger_Ptr := Expanded_Std_Logger'Access;

   --------------------------
   --  Buffer_Logger_Type  --
   --------------------------

   type Buffer_Logger_Type is new Logger_Type with
      record
         Buffer : Unbounded_String;
      end record;
   type Buffer_Logger_Ptr is access all Buffer_Logger_Type'Class;

   function New_Buffer_Logger return Buffer_Logger_Ptr;

   procedure Put_Always    (Log : in out Buffer_Logger_Type;
                            S   : in     String);

private

--    type Logger_Type is abstract tagged
--       record
--          Verbosity_Level : Natural := 0;
--          Indent_Level    : Natural := 0;
--       end record;

end Util.IO;
