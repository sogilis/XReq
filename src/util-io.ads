--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Sequential_IO;
with GNAT.OS_Lib;

use Ada.Strings.Unbounded;

package Util.IO is

   ---------------
   --  Char_IO  --
   ---------------

   package Char_IO is new Ada.Sequential_IO (Character);

   procedure Read_Whole_File (File   : in out Char_IO.File_Type;
                              Buffer : in out Unbounded_String);

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

   procedure Spawn          (Program_Name  : in     String;
                             Args          : in     GNAT.OS_Lib.Argument_List;
                             Output_Buffer : in out Unbounded_String;
                             Success       : out    Boolean;
                             Return_Code   : out    Integer;
                             Err_To_Out    : in     Boolean := True);

end Util.IO;
