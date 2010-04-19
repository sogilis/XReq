--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Directories;
with Util.IO;

use Ada.Strings.Unbounded;
use Ada.Strings;
use Ada.Strings.Fixed;
use Ada.Directories;
use Util.IO;

package body Test_Suite.Main is

   procedure Add_Tests (
      Ret : in AUnit.Test_Suites.Access_Test_Suite)
   is
   begin
--       Ret.Add_Test (new Test_1);
--       Ret.Add_Test (new Test_2);
      null;
   end Add_Tests;


   --  Main  ------------------------------------------------------------------

   function  Name (T : in Test_1) return String is
      pragma Unreferenced (T);
   begin
      return ("Main (1)");
   end Name;

   procedure Run (T : in out Test_1) is
      procedure SpawnAssert2 is new Spawn_Assert (Test_1);
   begin

      SpawnAssert2 (T, "-h");
      SpawnAssert2 (T, "tests/features/simplest.feature");
      SpawnAssert2 (T, "-otmp --step b tests/features/simplest.feature",
                    Expected_Result => False);

      SpawnAssert2 (T, "tests/features/simplest.feature " &
                    "tests/features/simplest2.feature");

      SpawnAssert2 (T, "-o /tmp",
                    Expected_Result => False);

      SpawnAssert2 (T, "--step=/tmp",
                    Expected_Result => False);

      SpawnAssert2 (T, "--lang fr",
                    Expected_Result => False);

      SpawnAssert2 (T, "--step 1 --step 2",
                    Expected_Result => False);

      SpawnAssert2 (T, "--step",
                    Expected_Result => False);

      SpawnAssert2 (T, "--lang=ada --step tests/features/step_definitions " &
                    "tests/features/simplest.feature",
                    Expected_Result => True);

      SpawnAssert2 (T, "--step a --step tests/features/step_definitions " &
                    "tests/features/simplest.feature",
                    Expected_Result => False);

      SpawnAssert2 (T, "--toto",
                    Expected_Result => False);

   end Run;

   --  Test_2  ----------------------------------------------------------------

   function  Name (T : in Test_2) return String is
      pragma Unreferenced (T);
   begin
      return ("Main (2)");
   end Name;

   procedure Run (T : in out Test_2) is
      procedure SpawnAssert2 is new Spawn_Assert (Test_2);
      Gnat_Flags : constant String := "-gnat05 -aI../../../src/lib";
   begin

      SpawnAssert2 (T, "-x result1 -k " &
                    "tests/features/simplest.feature " &
                    "tests/features/simplest2.feature");

      SpawnAssert2 (T,
                    Gnat_Flags & " -aI../step_definitions result1",
                    Directory       => "tests/features/tests",
                    Executable_Name => "gnatmake");
   end Run;

   --  Private  ---------------------------------------------------------------
   --  See <http://www.adacore.com/2008/11/24/gem-54/>

   procedure Spawn_Assert  (T               : in Test_Case_Generic_Type;
                            Argument_String : in String;
                            Expected_Result : in Boolean := True;
                            Directory       : in String := "";
                            Executable_Name : in String := "bin/xreq")
   is
      LF        : constant String := "" & ASCII.LF;
      Cmd_Line  : constant String := Executable_Name & " " & Argument_String;
      Buffer    : Unbounded_String;
      Ret_Code  : Integer;
      Success   : Boolean;
   begin
      if Directory = "" then
         Append (Buffer, "Current Directory: " & Current_Directory & LF);
      else
         Append (Buffer, "Current Directory: " & Directory & LF);
      end if;
      Append (Buffer, "Spawn: " & Cmd_Line & LF);
      Spawn (Executable_Name, Argument_String,
             Buffer, Success, Ret_Code, Directory);
      if Success then
         Append (Buffer, "Succeeded (" & Trim (Ret_Code'Img, Left));
      else
         Append (Buffer, "Failed (" & Trim (Ret_Code'Img, Left));
      end if;
      Append (Buffer, To_Unbounded_String ("): " & Cmd_Line & LF));
      Success := Ret_Code = 0;
      if Expected_Result then
         T.Assert (Success, "Failed: " & Cmd_Line & " (expected success):" &
                   LF & To_String (Buffer));
      else
         T.Assert (not Success, "Succeeded: " & Cmd_Line &
                   " (expected failure)" & LF & To_String (Buffer));
      end if;
   end Spawn_Assert;

end Test_Suite.Main;
