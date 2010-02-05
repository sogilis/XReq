--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Strings.Unbounded;

use Ada.Text_IO;
use Ada.Strings.Unbounded;

package Util.IO is

   BufferSize : Natural := 2000;

   function Get_Line (File : File_Type) return Unbounded_String;

end Util.IO;
