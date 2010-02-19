--                         Copyright (C) 2010, Sogilis                       --

with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Directories;
with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.OS_Lib;
with AUnit.Assertions;
with Util.IO;

use Ada.Exceptions;
use Ada.Strings.Unbounded;
use Ada.Strings;
use Ada.Strings.Fixed;
use Ada.Directories;
use Ada.Command_Line;
use Ada.Text_IO;
use GNAT.OS_Lib;
use AUnit.Assertions;
use Util.IO;

package body Test_Suite.Main is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
      Ret.Add_Test (new Test_1);
   end Add_Tests;


   --  Main  ------------------------------------------------------------------

   function  Name (T : in Test_1) return String is
      pragma Unreferenced (T);
   begin
      return ("Main");
   end Name;

   procedure Run (T : in out Test_1) is
      pragma Unreferenced (T);
   begin

      Title ("Main");

      Spawn_Assert ("-h");
      Spawn_Assert ("tests/features/simplest.feature");
      Spawn_Assert ("-otmp --step b tests/features/simplest.feature",
                    Expected_Result => False);

      Spawn_Assert ("tests/features/simplest.feature " &
                    "tests/features/simplest2.feature");

      Spawn_Assert ("-o /tmp",
                    Expected_Result => False);

      Spawn_Assert ("--step=/tmp",
                    Expected_Result => False);

      Spawn_Assert ("--lang fr",
                    Expected_Result => False);

      Spawn_Assert ("--step 1 --step 2",
                    Expected_Result => False);

      Spawn_Assert ("--step",
                    Expected_Result => False);

      Spawn_Assert ("--step tests/features/step_definitions " &
                    "tests/features/simplest.feature",
                    Expected_Result => True);

      Spawn_Assert ("--step a --step tests/features/step_definitions " &
                    "tests/features/simplest.feature",
                    Expected_Result => False);

      Spawn_Assert ("--toto",
                    Expected_Result => False);

      End_Test;
   exception
      when Error : others =>
         Put_Line (Exception_Information (Error));
         Reraise_Occurrence (Error);
         End_Test;
   end Run;

   --  Private  ---------------------------------------------------------------
   --  See <http://www.adacore.com/2008/11/24/gem-54/>

   procedure Spawn_Assert  (Argument_String : in String;
                            Expected_Result : in Boolean := True;
                            Executable_Name : in String := "adaspec")
   is
      LF        : constant String := "" & ASCII.LF;
      Buffer    : Unbounded_String;
      Ret_Code  : Integer;
      Success   : Boolean;
      Command   : constant String := Command_Path (Executable_Name);
      Cmd_Line  : constant String := Command_Line (Command, Argument_String);
      Arguments : constant Argument_List_Access :=
                  Argument_List (Argument_String);
   begin
      Append (Buffer, "Current Directory: " & Current_Directory & LF);
      Append (Buffer, "Spawn: " & Cmd_Line & LF);
      Spawn (Command, Arguments.all, Buffer, Success, Ret_Code);
      if Success then
         Append (Buffer, "Succeeded (" & Trim (Ret_Code'Img, Left));
      else
         Append (Buffer, "Failed (" & Trim (Ret_Code'Img, Left));
      end if;
      Append (Buffer, "): " & Cmd_Line & LF);
      Success := Ret_Code = 0;
      if Expected_Result then
         Assert (Success, "Failed: " & Cmd_Line & " (expected success):" & LF &
                 To_String (Buffer));
      else
         Assert (not Success, "Succeeded: " & Cmd_Line &
                 " (expected failure)" & LF & To_String (Buffer));
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
