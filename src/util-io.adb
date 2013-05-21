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

with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Ada.Environment_Variables;
with GNAT.OS_Lib;

use Ada.Strings.Fixed;
use GNAT.OS_Lib;

package body Util.IO is

   -------------------
   --  Logger_Type  --
   -------------------

   procedure Add_Verbosity (Log : in out Logger_Type;
                            V   : in     Integer)
   is
   begin
      Log.Verbosity_Level := Log.Verbosity_Level + V;
   end Add_Verbosity;

   function  Verbosity     (Log : in     Logger_Type)
                                  return Integer
   is
   begin
      return Log.Verbosity_Level;
   end Verbosity;

   procedure Put_Line      (Log : in out Logger_Type;
                            S   : in     String;
                            V   : in     Integer := 0)
   is
   begin
      Log.Put_Line (V, S);
   end Put_Line;

   procedure Put_Line      (Log : in out Logger_Type;
                            S   : in     Unbounded_String;
                            V   : in     Integer := 0)
   is
   begin
      Log.Put_Line (V, To_String (S));
   end Put_Line;

   procedure Put           (Log : in out Logger_Type;
                            S   : in     String;
                            V   : in     Integer := 0)
   is
   begin
      Log.Put (V, S);
   end Put;

   procedure Put           (Log : in out Logger_Type;
                            S   : in     Unbounded_String;
                            V   : in     Integer := 0)
   is
   begin
      Log.Put (V, To_String (S));
   end Put;

   procedure Put_Line      (Log : in out Logger_Type;
                            V   : in     Integer;
                            S   : in     String)
   is
   begin
      Log.Put_Indent (V);
      Log.Put        (V, S);
      Log.New_Line   (V);
   end Put_Line;

   procedure Put_Line      (Log : in out Logger_Type;
                            V   : in     Integer;
                            S   : in     Unbounded_String)
   is
   begin
      Log.Put_Line (V, To_String (S));
   end Put_Line;

   procedure Put           (Log : in out Logger_Type;
                            V   : in     Integer;
                            S   : in     String)
   is
      Self : constant Logger_Ptr := Log'Unchecked_Access;
   begin
      if V <= Log.Verbosity_Level then
         Self.Put_Always (S);
      end if;
   end Put;

   procedure Put           (Log : in out Logger_Type;
                            V   : in     Integer;
                            S   : in     Unbounded_String)
   is
   begin
      Log.Put (V, To_String (S));
   end Put;

   procedure Put_Indent    (Log : in out Logger_Type;
                            V   : in     Integer := 0)
   is
   begin
      Log.Put (V, String'(Log.Indent_Level * " "));
   end Put_Indent;

   procedure New_Line      (Log : in out Logger_Type;
                            V   : in     Integer := 0)
   is
   begin
      Log.Put (V, "" & ASCII.LF);
   end New_Line;

   procedure Indent        (Log : in out Logger_Type;
                            N   : in     Natural := 3)
   is
   begin
      Log.Indent_Level := Log.Indent_Level + N;
   end Indent;

   procedure UnIndent      (Log : in out Logger_Type;
                            N   : in     Natural := 3)
   is
   begin
      Log.Indent_Level := Log.Indent_Level - N;
   end UnIndent;

   procedure Free          (Log : in out Logger_Ptr) is
      procedure DeAlloc is new
         Ada.Unchecked_Deallocation (Logger_Type'Class, Logger_Ptr);
   begin
      DeAlloc (Log);
   end Free;

   ------------------------
   --  Null_Logger_Type  --
   ------------------------

   function New_Null_Logger return Null_Logger_Ptr is
   begin
      return new Null_Logger_Type;
   end New_Null_Logger;

   ----------------------------
   --  Standard_Logger_Type  --
   ----------------------------

   function New_Standard_Logger return Standard_Logger_Ptr is
   begin
      return new Standard_Logger_Type;
   end New_Standard_Logger;

   procedure Put_Always    (Log : in out Standard_Logger_Type;
                            S   : in     String)
   is
      pragma Unreferenced (Log);
   begin
      Ada.Text_IO.Put (S);
   end Put_Always;

   --------------------------
   --  Buffer_Logger_Type  --
   --------------------------

   function New_Buffer_Logger return Buffer_Logger_Ptr is
   begin
      return new Buffer_Logger_Type;
   end New_Buffer_Logger;

   procedure Put_Always    (Log : in out Buffer_Logger_Type;
                            S   : in     String)
   is
   begin
      Append (Log.Buffer, S);
   end Put_Always;

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

   procedure Append_File (File_Name : in String;
                          Content   : in String)
   is
      use Char_IO;
      File : Char_IO.File_Type;
   begin
      Open (File, Append_File, File_Name);
      for I in Content'Range loop
         Write (File, Content (I));
      end loop;
      Close (File);
   end Append_File;

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
      --  use Ada.Directories;
      use Char_IO;
      Tmp_File : File_Type;
      Cmd_Path : GNAT.OS_Lib.String_Access;
   begin
      Create (Tmp_File);
      --  Ada.Text_IO.Put_Line ("Temp file: " & Name (Tmp_File));
      if Index (Command_Name, "/") not in Command_Name'Range then
         Cmd_Path := Locate_Exec_On_Path (Command_Name);
         if Cmd_Path = null then
            raise Ada.IO_Exceptions.Name_Error
               with "Cannot locate """ & Command_Name & """ on the PATH";
         end if;
         Spawn  (Cmd_Path.all,
                 Args,
                 Name (Tmp_File),
                 Success,
                 Return_Code,
                 Err_To_Out);
      else
         Spawn  (Command_Name,
                 Args,
                 Name (Tmp_File),
                 Success,
                 Return_Code,
                 Err_To_Out);
      end if;
      --  Ada.Text_IO.Put_Line ("Result " & Command_Name & " " &
      --     Success'Img & Return_Code'Img);
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


   procedure System         (Script        : in String;
                             Output_Buffer : in out Unbounded_String;
                             Return_Code   : out    Integer;
                             Err_To_Out    : in     Boolean := True)
   is
      Arg0    : aliased constant String := "sh";
      Arg1    : aliased String := "-c";
      Arg2    : aliased String := Script;
      Args    : constant Argument_List (1 .. 2)
              := (Arg1'Unchecked_Access, Arg2'Unchecked_Access);
      Success : Boolean;
   begin
      Spawn (Arg0, Args, Output_Buffer, Success, Return_Code, Err_To_Out);
      if not Success then
         --  GCOV_IGNORE_BEGIN
         --  Should never happen
         raise Ada.IO_Exceptions.Name_Error with "could not execute shell";
         --  GCOV_IGNORE_END
      end if;
   end System;

   --------------
   --  GetEnv  --
   --------------

   function GetEnv (VarName, Default : in String) return String is
      use Ada.Environment_Variables;
   begin
      if Exists (VarName) then
         return Value (VarName);
      else
         return Default;
      end if;
   end GetEnv;


end Util.IO;
