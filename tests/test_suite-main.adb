--                         Copyright (C) 2010, Sogilis                       --

--  with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Directories;
with Ada.Command_Line;
with GNAT.OS_Lib;
with AUnit.Assertions;

--  use Ada.Strings.Unbounded;
use Ada.Text_IO;
use Ada.Directories;
use Ada.Command_Line;
use GNAT.OS_Lib;
use AUnit.Assertions;

package body Test_Suite.Main is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_1);
   end Add_Tests;


   --  Main  ------------------------------------------------------------------

   function  Name (T : in Test_1) return AUnit.Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Main");
   end Name;

   procedure Run_Test (T : in out Test_1) is
      pragma Unreferenced (T);
   begin

      Spawn_Assert (Argument_String => "-h");
      Spawn_Assert (Argument_String => "tests/features/file.feature");
      Spawn_Assert (Argument_String => "-oa --step b a/file.feature");

      Spawn_Assert (Argument_String => "tests/features/file.feature " &
                                       "tests/features/file.feature",
                    Expected_Result => False);

      Spawn_Assert (Argument_String => "-o /tmp",
                    Expected_Result => False);

      Spawn_Assert (Argument_String => "--step=/tmp",
                    Expected_Result => False);

      Spawn_Assert (Argument_String => "--lang fr",
                    Expected_Result => False);

      Spawn_Assert (Argument_String => "--step 1 --step 2",
                    Expected_Result => False);

      Spawn_Assert (Argument_String => "--step",
                    Expected_Result => False);

      Spawn_Assert (Argument_String => "--toto",
                    Expected_Result => False);
   end Run_Test;

   --  Private  ---------------------------------------------------------------
   --  See <http://www.adacore.com/2008/11/24/gem-54/>

   procedure Spawn_Assert  (Argument_String : in String;
                            Expected_Result : in Boolean := True;
                            Executable_Name : in String := "adaspec") is
      Success   : Boolean;
      Command   : constant String := Command_Path (Executable_Name);
      Cmd_Line  : constant String := Command_Line (Command, Argument_String);
      Arguments : constant Argument_List_Access :=
                  Argument_List (Argument_String);
   begin
      Put_Line ("Current Directory: " & Current_Directory);
      Put_Line ("Spawn: " & Cmd_Line);
      Spawn (Command, Arguments.all, Success);
      if Success then
         Put_Line ("Succeeded: " & Cmd_Line);
      else
         Put_Line ("Failed: " & Cmd_Line);
      end if;
      if Expected_Result then
         Assert (Success,     "Failed: " & Cmd_Line & " (expected success)");
      else
         Assert (not Success, "Succeeded: " & Cmd_Line &
                              " (expected failure)");
      end if;
   end Spawn_Assert;

   function Command_Path (Executable_Name : in String := "adaspec")
      return String
   is
   begin
      return Compose (Containing_Directory (Command_Name), Executable_Name);
   end Command_Path;

   function Command_Line (Executable_Path : in String;
                          Argument_String : in String) return String is
   begin
      return Executable_Path & " " & Argument_String;
   end Command_Line;

   function Argument_List (Command_Line   : in String)
      return Argument_List_Access
   is
   begin
      return Argument_String_To_List (Command_Line);
   end Argument_List;

end Test_Suite.Main;
