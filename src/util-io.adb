--                         Copyright (C) 2010, Sogilis                       --

with GNAT.OS_Lib;

use GNAT.OS_Lib;

package body Util.IO is

   ---------------
   --  Char_IO  --
   ---------------

   procedure Read_Whole_File (File   : in out Char_IO.File_Type;
                              Buffer : in out Unbounded_String) is
      use Char_IO;
      Char   : Character;
   begin
      Reset (File, In_File);
      while not End_Of_File (File) loop
         Read   (File, Char);
         Append (Buffer, Char);
      end loop;
   end Read_Whole_File;

   -----------------------------------
   --  Text_IO  --  Get_Whole_Line  --
   -----------------------------------

   --  Thanks to WikiBooks
   --  <http://en.wikibooks.org/wiki/Ada_Programming/Libraries/Ada.Text_IO>
   function Get_Whole_Line (File : in Ada.Text_IO.File_Type)
                               return Unbounded_String
   is
      use Ada.Text_IO;
      Retval : Unbounded_String := Null_Unbounded_String;
      Item   : String (1 .. BufferSize);
      Last   : Natural;
   begin
      Get_Whole_Line : loop

         Get_Line (File, Item, Last);
         Append   (Retval, Item (1 .. Last));

         exit Get_Whole_Line when Last < Item'Last or End_Of_File (File);

      end loop Get_Whole_Line;
      return Retval;
   end Get_Whole_Line;

   function Get_Whole_Line (File : in Ada.Text_IO.File_Type)
                               return String
   is
      use Ada.Text_IO;
   begin
      return To_String (Get_Whole_Line (File));
   end Get_Whole_Line;

   function Read_Whole_File (File_Name : in String;
                             CRLF      : in String := "" & ASCII.LF)
                                         return String
   is
      use Ada.Text_IO;
      Content : Unbounded_String;
      Line    : Unbounded_String;
      File    : File_Type;
   begin
      Open (File, In_File, File_Name);
      loop
         Line := Get_Whole_Line (File);
         Append (Content, Line);
         Append (Content, CRLF);
      end loop;
   exception
      when End_Error =>
         Close (File);
         return To_String (Content);
   end Read_Whole_File;

   procedure Spawn (Program_Name  : in     String;
                    Args          : in     Argument_List;
                    Output_Buffer : in out Unbounded_String;
                    Success       : out    Boolean;
                    Return_Code   : out    Integer;
                    Err_To_Out    : in     Boolean := True)
   is
      use Char_IO;
      Tmp_File  : File_Type;
   begin
      Create (Tmp_File);
      Spawn  (Program_Name,
              Args,
              Name (Tmp_File),
              Success,
              Return_Code,
              Err_To_Out);
      Read_Whole_File (Tmp_File, Output_Buffer);
      Delete (Tmp_File);
   end Spawn;


end Util.IO;
