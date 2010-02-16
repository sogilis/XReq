--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Strings.Unbounded;

use Ada.Text_IO;
use Ada.Strings.Unbounded;

package Util.IO is

   BufferSize : Natural := 2000;

   function Get_Whole_Line (File : in File_Type) return Unbounded_String;

   function Get_Whole_Line (File : in File_Type) return String;

   function Read_Whole_File (File_Name : in String;
                             CRLF      : in String := ASCII.CR & ASCII.LF)
                                         return String;

end Util.IO;
