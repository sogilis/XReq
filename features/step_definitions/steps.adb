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
with Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Ada.Directories;
with Ada.Sequential_IO;
with Ada.Text_IO;
with Ada.IO_Exceptions;
with GNAT.OS_Lib;
with XReqLib.Asserts;

use Ada.Strings.Unbounded;
use Ada.Directories;
use XReqLib.Asserts;

package body Steps is

   Last_Exit_Code : Integer := 0;
   Last_Command_Output : Unbounded_String;
   XReq_Dir : Unbounded_String := To_Unbounded_String (Current_Directory);

   package Char_IO is new Ada.Sequential_IO (Character);
   package ENV renames Ada.Environment_Variables;

   procedure Execute (Command_Line : in String) is
      use Char_IO;
      use GNAT.OS_Lib;
      Tmp_File      : Char_IO.File_Type;
      Success       : Boolean;
      Return_Code   : Integer;
      Char          : Character;
      Arg1          : aliased String := "-c";
      Arg2          : aliased String := "( " & Command_Line & ") 2>&1";
      Arg_List      : Argument_List (1 .. 2)  --  OK, I do an unchecked access
                    := (                      --  access here, but is it
                     Arg1'Unchecked_Access,   --  worse than an
                     Arg2'Unchecked_Access);  --  Unchecked_Deallocation?
   begin
      Create (Tmp_File);
      Last_Command_Output := Null_Unbounded_String;
      --  Append (Last_Command_Output, Arg3 & ASCII.LF);
      --  Append (Last_Command_Output, Name (Tmp_File) & ASCII.LF);
      Spawn  ("/bin/sh",
              Arg_List,
              Name (Tmp_File),
              Success,
              Return_Code,
              True);
      Assert (Success, "Spawn failed");
      Reset (Tmp_File, In_File);
      while not End_Of_File (Tmp_File) loop
         Read   (Tmp_File, Char);
         Append (Last_Command_Output, Char);
      end loop;
      Delete (Tmp_File);
      Last_Exit_Code := Return_Code;
   end Execute;

   ----------------------------------------------------------------------------

   procedure XReq_in_path (Args : in out Arg_Type) is
   begin
      if ENV.Exists ("PATH") then
         ENV.Set ("PATH", To_String (XReq_Dir) & "/bin:" &
                  ENV.Value ("PATH"));
      else
         ENV.Set ("PATH", To_String (XReq_Dir) & "/bin");
      end if;
      if ENV.Exists ("LD_LIBRARY_PATH") then
         ENV.Set ("LD_LIBRARY_PATH", To_String (XReq_Dir) & "/lib/debug:" &
                  ENV.Value ("LD_LIBRARY_PATH"));
      else
         ENV.Set ("LD_LIBRARY_PATH", To_String (XReq_Dir) & "/lib/debug");
      end if;
      if ENV.Exists ("GPR_PROJECT_PATH") then
         ENV.Set ("GPR_PROJECT_PATH", To_String (XReq_Dir) & ":" &
                  ENV.Value ("GPR_PROJECT_PATH"));
      else
         ENV.Set ("GPR_PROJECT_PATH", To_String (XReq_Dir));
      end if;
      if ENV.Exists ("ADA_INCLUDE_PATH") then
         ENV.Set ("ADA_INCLUDE_PATH", To_String (XReq_Dir) & "/src/lib:" &
                  To_String (XReq_Dir) & "/src/lib/dynamic:" &
                  ENV.Value ("ADA_INCLUDE_PATH"));
      else
         ENV.Set ("ADA_INCLUDE_PATH", To_String (XReq_Dir) & "/src/lib" &
                  To_String (XReq_Dir) & "/src/lib/dynamic");
      end if;
      ENV.Set ("ADA_INCLUDE_PATH", To_String (XReq_Dir) &
               "/src/common:" & ENV.Value ("ADA_INCLUDE_PATH"));
      if ENV.Exists ("C_INCLUDE_PATH") then
         ENV.Set ("C_INCLUDE_PATH", To_String (XReq_Dir) & "/src/lib:" &
                  ENV.Value ("C_INCLUDE_PATH"));
      else
         ENV.Set ("C_INCLUDE_PATH", To_String (XReq_Dir) & "/src/lib");
      end if;
      if ENV.Exists ("LIBRARY_PATH") then
         ENV.Set ("LIBRARY_PATH", To_String (XReq_Dir) & "/lib/debug:" &
                  ENV.Value ("LIBRARY_PATH"));
      else
         ENV.Set ("LIBRARY_PATH", To_String (XReq_Dir) & "/lib/debug");
      end if;
   end XReq_in_path;

   procedure Given_the_sources_of_XReq_are_in_path (Args : in out Arg_Type)
   is
   begin
      if ENV.Exists ("ADA_INCLUDE_PATH") then
         ENV.Set ("ADA_INCLUDE_PATH", To_String (XReq_Dir) & "/src:" &
                  ENV.Value ("ADA_INCLUDE_PATH"));
      else
         ENV.Set ("ADA_INCLUDE_PATH", To_String (XReq_Dir) & "/src");
      end if;
   end Given_the_sources_of_XReq_are_in_path;

   procedure Given_I_am_in_xreq_dir (Args : in out Arg_Type) is
   begin
      Set_Directory (To_String (XReq_Dir));
   end Given_I_am_in_xreq_dir;

   procedure Given_I_am_in (Args : in out Arg_Type) is
   begin
      Set_Directory (Args.Match (1));
   end Given_I_am_in;

   procedure I_am_empty_dir (Args : in out Arg_Type) is
      Tmp_Dir : constant String := Compose (To_String (XReq_Dir), "tmp");
   begin
      begin
         Delete_Tree (Tmp_Dir);
      exception
         when Name_Error => null;
      end;
      Create_Path (Tmp_Dir);
      Set_Directory (Tmp_Dir);
   end I_am_empty_dir;

   procedure Given_a_file (Args : in out Arg_Type) is
      --  use Ada.Text_IO;
      use Char_IO;
      File_Name : constant String := Args.Match (1);
      Text      : constant String := Args.Text;
      File      : Char_IO.File_Type;
      --  File      : File_Type;
      --  Text_File : Ada.Text_IO.File_Type;
      Buffer    : Unbounded_String;
   begin
      Create_Path (Containing_Directory (File_Name));
      Create (File, Out_File, File_Name);
      for I in Text'Range loop
         Write (File, Text (I));
      end loop;
      Close (File);
      --  ---------------------------------------------------------------------
      --  Create (File, Out_File, File_Name);
      --  for I in Text'Range loop
      --     Put (File, Text (I));
      --  end loop;
      --  Flush (File);
      --  Close (File);
      --  ---------------------------------------------------------------------
      --  It seems that the file is not completely written after Close, and it
      --  takes few miliseconds for it to be written. in the meantime, if we
      --  want to read its content, we risk reading its old content. So we try
      --  a forced flush.
      --  ---------------------------------------------------------------------
      --  delay 10.0;
      --  ---------------------------------------------------------------------
      --  Ada.Text_IO.Open  (Text_File, Ada.Text_IO.Append_File, File_Name);
      --  Ada.Text_IO.Flush (Text_File);
      --  Ada.Text_IO.Close (Text_File);
      --  ---------------------------------------------------------------------
      --  Open (File, In_File, File_Name);
      --  while not End_Of_File (File) loop
      --     Read (File, Char);
      --     Append (Buffer, Char);
      --  end loop;
      --  Close (File);
      --  Assert (Buffer = Text, "File incorrectly written");
   end Given_a_file;

   procedure I_run_xreq (Args : in out Arg_Type) is
      Buffer : Unbounded_String;
      procedure FillEnv (Name, Value : String) is
      begin
         Append(Buffer, Name & "=" & Value & ASCII.LF);
      end FillEnv;
   begin
      Ada.Environment_Variables.Iterate (FillEnv'Access);
      Args.Add_Para ("Current Directory:");
      Args.Add_Text (Current_Directory);
      Execute ("xreq " & Args.Match (1));
      Args.Add_Para ("Exit code:" & Last_Exit_Code'Img);
      Args.Add_Para ("Command Output:");
      Args.Add_Text (To_String (Last_Command_Output));
      Args.Add_Para ("Environment Variables:");
      Args.Add_Text (To_String (Buffer));
   end I_run_xreq;

   procedure it_should_pass_fail (Args : in out Arg_Type) is
   begin
      Args.Add_Para ("Exit code:" & Last_Exit_Code'Img);
      Args.Add_Para ("Command output:");
      Args.Add_Text (To_String (Last_Command_Output));
      if Args.Match (1) = "pass" then
         Assert (Last_Exit_Code = 0, "failed with code" & Last_Exit_Code'Img);
      else
         Assert (Last_Exit_Code /= 0, "succeed");
      end if;
   end it_should_pass_fail;

   procedure it_should_pass_fail_with (Args : in out Arg_Type) is
   begin
      it_should_pass_fail(Args);
--       Args.Add_Para ("Expected output:");
--       Args.Add_Text (Args.Text);
--       Args.Add_Para ("Command output:");
--       Args.Add_Text (To_String (Last_Command_Output));
      Assert (Args.Text = To_String (Last_Command_Output),
              "Unexpected command output");
   end it_should_pass_fail_with;

   procedure when_I_compile_in (Args : in out Arg_Type) is
      Dir      : constant String := Current_Directory;
      Cmd_Line : constant String :=
         "gnatmake -m -g -E -gnata -gnat05 -aI../step_definitions " &
         Args.Match (1);
   begin
      Set_Directory (Args.Match (2));
      Args.Add_Para ("Current Directory:");
      Args.Add_Text (Current_Directory);
      Args.Add_Para ("Command Line:");
      Args.Add_Text (Cmd_Line);
      Execute (Cmd_Line);
      Set_Directory (Dir);
   end when_I_compile_in;

   procedure when_I_run_in (Args : in out Arg_Type) is
      Dir : constant String := Current_Directory;
   begin
      Set_Directory (Args.Match (2));
      Args.Add_Para ("Current Directory:");
      Args.Add_Text (Current_Directory);
      Execute (Args.Match (1));
      Set_Directory (Dir);
   end when_I_run_in;

   procedure when_I_run_the_test_suite_in (Args : in out Arg_Type) is
      use Ada.Strings.Fixed;
      use Ada.Strings;
      Dir : constant String := Current_Directory;
   begin
      begin
         Set_Directory (Args.Match (2));
         Args.Add_Para ("Current Directory:");
         Args.Add_Text (Current_Directory);
      exception
         when Constraint_Error =>
            null;
      end;
      Execute (Args.Match (1) & " --no-color --no-stacktrace");
      Set_Directory (Dir);
      declare
         Output : constant String := To_String (Last_Command_Output);
         I      : Integer;
      begin
         I := Index (Output, "Finished in ", Backward);
         if I /= 0 and then Index (Output, "" & ASCII.LF, I) = Output'Last then
            Args.Add_Para ("Removed last line from output:");
            Args.Add_Text (Output (I .. Output'Last));
            Last_Command_Output := To_Unbounded_String
               (Output (Output'First .. I - 1));
         else
            Args.Add_Para ("Could not find ""Finished in "" in output");
            Args.Add_Para ("Command output:");
            Args.Add_Text (Output);
         end if;
      end;
   end when_I_run_the_test_suite_in;

   procedure when_I_run (Args : in out Arg_Type) is
   begin
      Execute (Args.Match (1));
      Args.Add_Para ("Exit code:" & Last_Exit_Code'Img);
      Args.Add_Para ("Current Directory:");
      Args.Add_Text (Current_Directory);
      Args.Add_Para ("Command output:");
      Args.Add_Text (To_String (Last_Command_Output));
   end when_I_run;

   procedure Then_file_should_exist (Args : in out Arg_Type) is
   begin
      Args.Add_Para ("Current Directory:");
      Args.Add_Text (Current_Directory);
      Assert (Exists (Args.Match (1)),
              "File should exists: " & Args.Match (1));
   end Then_file_should_exist;

   procedure Then_file_should_not_exist (Args : in out Arg_Type) is
   begin
      Args.Add_Para ("Current Directory:");
      Args.Add_Text (Current_Directory);
      Assert (not Exists (Args.Match (1)),
              "File should not exists: " & Args.Match (1));
   end Then_file_should_not_exist;

   procedure Then_the_output_should_contain (Args : in out Arg_Type) is
   begin
      Args.Add_Para ("Command output:");
      Args.Add_Text (To_String (Last_Command_Output));
      if Index (Last_Command_Output, Args.Text) = 0 then
         Assert (False, "The output doesn't match");
      end if;
   end Then_the_output_should_contain;

   procedure Then_the_output_should_not_contain (Args : in out Arg_Type) is
   begin
      Args.Add_Para ("Command output:");
      Args.Add_Text (To_String (Last_Command_Output));
      if Index (Last_Command_Output, Args.Text) /= 0 then
         Assert (False, "The output match");
      end if;
   end Then_the_output_should_not_contain;

   procedure Then_the_file_should_contain (Args : in out Arg_Type) is
      use Char_IO;
      File      : Char_IO.File_Type;
      File_Name : constant String := Args.Match (1);
      Buffer    : Unbounded_String;
      Char      : Character;
   begin
      Open (File, In_File, File_Name);
      while not End_Of_File (File) loop
         Read   (File, Char);
         Append (Buffer, Char);
      end loop;
      Close (File);
      Args.Add_Para ("File content:");
      Args.Add_Text (To_String (Buffer));
      if Index (Buffer, Args.Text) = 0 then
         Assert (False, "The file doesn't match");
      end if;
   end Then_the_file_should_contain;


   procedure Not_Yet_Implemented (Args : in out Arg_Type) is
   begin
      Assert (False, "Not Yet Implemented");
   end Not_Yet_Implemented;

   procedure Given_is_empty (Args : in out Arg_Type) is
      Search : Search_Type;
      Entryy : Directory_Entry_Type;
   begin
      Start_Search (Search, Args.Match (1), "");
      while More_Entries (Search) loop
         Get_Next_Entry (Search, Entryy);
         case Kind (Entryy) is
            when Directory =>
               if Simple_Name (Entryy) /= "." and Simple_Name (Entryy) /= ".."
               then
                  Delete_Tree (Full_Name (Entryy));
               end if;
            when others =>
               Delete_File (Full_Name (Entryy));
         end case;
      end loop;
      End_Search (Search);
   end Given_is_empty;

end Steps;
