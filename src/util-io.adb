--                         Copyright (C) 2010, Sogilis                       --

with Ada.Directories;
with GNAT.OS_Lib;

use GNAT.OS_Lib;

package body Util.IO is

   -----------------
   --  Temp_Name  --
   -----------------

   function Temp_Name return String is
      use Char_IO;
      File : Char_IO.File_Type;
   begin
      Create (File, Out_File);
      declare
         N : constant String := Name (File);
      begin
         Delete (File);
         return N;
      end;
   end Temp_Name;

   ---------------
   --  Char_IO  --
   ---------------

   procedure Read_Whole_File (File   : in out Char_IO.File_Type;
                              Buffer : in out Unbounded_String) is
      use Char_IO;
      Char : Character;
   begin
      Reset (File, In_File);
      while not End_Of_File (File) loop
         Read   (File, Char);
         Append (Buffer, Char);
      end loop;
   end Read_Whole_File;

   procedure Write_Whole_File (File   : in out Char_IO.File_Type;
                               Buffer : in String)
   is
      use Char_IO;
   begin
      Reset (File, Out_File);
      for I in Buffer'Range loop
         Write (File, Buffer (I));
      end loop;
   end Write_Whole_File;


   function  Get_File (File_Name : in String) return String
   is
      use Char_IO;
      File : Char_IO.File_Type;
      Buff : Unbounded_String;
   begin
      Open (File, In_File, File_Name);
      Read_Whole_File (File, Buff);
      Close (File);
      return To_String (Buff);
   end Get_File;

   procedure Set_File (File_Name : in String;
                       Content   : in String)
   is
      use Char_IO;
      File : Char_IO.File_Type;
   begin
      Create (File, Out_File, File_Name);
      Write_Whole_File (File, Content);
      Close (File);
   end Set_File;

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

   -------------
   --  Spawn  --
   -------------

   procedure Spawn (Command_Name  : in     String;
                    Args          : in     Argument_List;
                    Output_Buffer : in out Unbounded_String;
                    Success       : out    Boolean;
                    Return_Code   : out    Integer;
                    Err_To_Out    : in     Boolean := True)
   is
--       use Ada.Directories;
      use Char_IO;
      Tmp_File : File_Type;
      Cmd_Path : GNAT.OS_Lib.String_Access
               := Locate_Exec_On_Path (Command_Name);
   begin
      Create (Tmp_File);
--       Ada.Text_IO.Put_Line ("Temp file: " & Name (Tmp_File));
      Spawn  (Cmd_Path.all,
              Args,
              Name (Tmp_File),
              Success,
              Return_Code,
              Err_To_Out);
--       Ada.Text_IO.Put_Line ("Result " & Command_Name & " " &
--          Success'Img & Return_Code'Img);
      Read_Whole_File (Tmp_File, Output_Buffer);
      Delete (Tmp_File);
      Free (Cmd_Path);
   end Spawn;

   procedure Spawn (Command_Name  : in     String;
                    Command_Line  : in     String;
                    Output_Buffer : in out Unbounded_String;
                    Success       : out    Boolean;
                    Return_Code   : out    Integer;
                    Directory     : in     String := "")
   is
      use Ada.Directories;
      Curr_Dir  : constant String := Current_Directory;
      Arguments : constant Argument_List_Access :=
                  Argument_String_To_List (Command_Line);
   begin
      if Directory /= "" then
         Set_Directory (Directory);
      end if;
      Util.IO.Spawn (Command_Name  => Command_Name,
                     Args          => Arguments.all,
                     Output_Buffer => Output_Buffer,
                     Success       => Success,
                     Return_Code   => Return_Code,
                     Err_To_Out    => True);
      if Directory /= "" then
         Set_Directory (Curr_Dir);
      end if;
   end Spawn;

end Util.IO;
