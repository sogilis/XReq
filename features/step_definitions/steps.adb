--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Environment_Variables;
with Ada.Directories;
with Ada.Sequential_IO;
with Ada.IO_Exceptions;
with GNAT.OS_Lib;

use Ada.Strings.Unbounded;
use Ada.Directories;

package body Steps is

   Last_Exit_Code : Integer := 0;
   Last_Command_Output : Unbounded_String;
   AdaSpec_Dir : Unbounded_String := To_Unbounded_String (Current_Directory);

   package Char_IO is new Ada.Sequential_IO (Character);
   package ENV renames Ada.Environment_Variables;

   procedure Execute (Command, Arguments : in String) is
      use Char_IO;
      use GNAT.OS_Lib;
      use Ada.Strings.Fixed;
      Tmp_File      : Char_IO.File_Type;
      Argument_List : Argument_List_Access :=
                      Argument_String_To_List (Arguments);
      Cmd_Path      : GNAT.OS_Lib.String_Access;
      Success       : Boolean;
      Return_Code   : Integer;
      Char          : Character;
   begin
      Create (Tmp_File);
      if Index (Command, "/") not in Command'Range then
         Cmd_Path := Locate_Exec_On_Path (Command);
         if Cmd_Path = null then
            raise Ada.IO_Exceptions.Name_Error
               with "Cannot locate """ & Command & """ on the PATH";
         end if;
         Spawn  (Cmd_Path.all,
                 Argument_List.all,
                 Name (Tmp_File),
                 Success,
                 Return_Code,
                 True);
      else
         Spawn  (Command,
                 Argument_List.all,
                 Name (Tmp_File),
                 Success,
                 Return_Code,
                 True);
      end if;
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
   end Execute;

   procedure Execute (Command_Line : in String) is
      use Ada.Strings.Fixed;
      I, J : Natural;
   begin
      I := Index (Command_Line, " ");
      if I = 0 then
         Execute (Command_Line, "");
      else
         J := Index_Non_Blank (Command_Line, I);
         Execute (Command_Line (Command_Line'First .. I - 1),
                  Command_Line (J .. Command_Line'Last));
      end if;
   end Execute;

   ----------------------------------------------------------------------------

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
      if ENV.Exists ("ADA_INCLUDE_PATH") then
         ENV.Set ("ADA_INCLUDE_PATH", To_String (AdaSpec_Dir) & "/src/lib:" &
                  ENV.Value ("ADA_INCLUDE_PATH"));
      else
         ENV.Set ("ADA_INCLUDE_PATH", To_String (AdaSpec_Dir) & "/src/lib");
      end if;
   end AdaSpec_in_path;

   procedure Given_the_sources_of_Adaspec_are_in_path (Args : in out Arg_Type)
   is
   begin
      if ENV.Exists ("ADA_INCLUDE_PATH") then
         ENV.Set ("ADA_INCLUDE_PATH", To_String (AdaSpec_Dir) & "/src:" &
                  ENV.Value ("ADA_INCLUDE_PATH"));
      else
         ENV.Set ("ADA_INCLUDE_PATH", To_String (AdaSpec_Dir) & "/src");
      end if;
   end Given_the_sources_of_Adaspec_are_in_path;

   procedure Given_I_am_in_adaspec_dir (Args : in out Arg_Type) is
   begin
      Set_Directory (To_String (AdaSpec_Dir));
   end Given_I_am_in_adaspec_dir;

   procedure Given_I_am_in (Args : in out Arg_Type) is
   begin
      Set_Directory (Args.Match (1));
   end Given_I_am_in;

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
   begin
      Args.Add_Para ("Current Directory:");
      Args.Add_Text (Current_Directory);
      Execute ("adaspec", Args.Match (1));
   end I_run_adaspec;

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
      Args.Add_Para ("Command output:");
      Args.Add_Text (To_String (Last_Command_Output));
      Assert (Args.Text = To_String (Last_Command_Output),
              "Unexpected command output");
   end it_should_pass_fail_with;

   procedure when_I_compile_in (Args : in out Arg_Type) is
      Dir      : constant String := Current_Directory;
      Cmd_Line : constant String :=
         "gnatmake -g -E -gnata -gnat05 -aI../step_definitions " &
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

   procedure when_I_run (Args : in out Arg_Type) is
   begin
      Execute (Args.Match (1));
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

end Steps;
