--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Ada.Environment_Variables;
with Ada.Directories;
with Ada.Sequential_IO;
with GNAT.OS_Lib;

use Ada.Strings.Unbounded;
use Ada.Directories;

package body Steps is

   Last_Exit_Code : Integer := 0;
   Last_Command_Output : Unbounded_String;
   AdaSpec_Dir : Unbounded_String := To_Unbounded_String (Current_Directory);

   package Char_IO is new Ada.Sequential_IO (Character);
   package ENV renames Ada.Environment_Variables;

   procedure AdaSpec_in_path (Args : in out Arg_Type) is
   begin
      if ENV.Exists ("PATH") then
         ENV.Set ("PATH", To_String (AdaSpec_Dir) & "/bin:" &
                  ENV.Value ("PATH"));
      else
         ENV.Set ("PATH", To_String (AdaSpec_Dir) & "/bin");
      end if;
      if ENV.Exists ("GPR_PROJECT_PATH") then
         ENV.Set ("GPR_PROJECT_PATH", To_String (AdaSpec_Dir) & ":" &
                  ENV.Value ("GPR_PROJECT_PATH"));
      else
         ENV.Set ("GPR_PROJECT_PATH", To_String (AdaSpec_Dir));
      end if;
   end AdaSpec_in_path;

   procedure I_am_empty_dir (Args : in out Arg_Type) is
      Tmp_Dir : constant String := Compose (To_String (AdaSpec_Dir), "tmp");
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
      use Char_IO;
      File      : Char_IO.File_Type;
      File_Name : constant String := Args.Match (1);
      Text      : constant String := Args.Text;
   begin
      Create_Path (Containing_Directory (File_Name));
      Create (File, Out_File, File_Name);
      for I in Text'Range loop
         Write (File, Text (I));
      end loop;
      Close (File);
   end Given_a_file;

   procedure I_run_adaspec (Args : in out Arg_Type) is
      use Char_IO;
      use GNAT.OS_Lib;
      Tmp_File      : Char_IO.File_Type;
      Arguments     : constant String := Args.Match (1);
      Argument_List : Argument_List_Access :=
                      Argument_String_To_List (Arguments);
      Cmd_Path      : GNAT.OS_Lib.String_Access;
      Success       : Boolean;
      Return_Code   : Integer;
      Char          : Character;
   begin
      Create (Tmp_File);
      Cmd_Path := Locate_Exec_On_Path ("adaspec");
      Spawn  (Cmd_Path.all,
              Argument_List.all,
              Name (Tmp_File),
              Success,
              Return_Code,
              True);
      Assert (Success, "Spawn failed");
      Free (Cmd_Path);
      Free (Argument_List);
      Reset (Tmp_File, In_File);
      Last_Command_Output := Null_Unbounded_String;
      while not End_Of_File (Tmp_File) loop
         Read   (Tmp_File, Char);
         Append (Last_Command_Output, Char);
      end loop;
      Delete (Tmp_File);
      Last_Exit_Code := Return_Code;
   end I_run_adaspec;

   procedure it_should_pass_fail (Args : in out Arg_Type) is
   begin
      if Args.Match (1) = "pass" then
         Assert (Last_Exit_Code = 0, "failed with code" & Last_Exit_Code'Img);
      else
         Assert (Last_Exit_Code /= 0, "succeed");
      end if;
   end it_should_pass_fail;

   procedure it_should_pass_fail_with (Args : in out Arg_Type) is
   begin
      it_should_pass_fail(Args);
      Assert (Args.Text = Last_Command_Output, "wrong output");
   end it_should_pass_fail_with;

   procedure Not_Yet_Implemented (Args : in out Arg_Type) is
   begin
      Assert (False, "Not Yet Implemented");
   end Not_Yet_Implemented;

end Steps;
