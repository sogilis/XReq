--                         Copyright (C) 2010, Sogilis                       --

--  with Ada.Strings.Unbounded;
with Ada.Directories;
with Ada.Command_Line;
with GNAT.OS_Lib;
with AUnit.Assertions;

--  use Ada.Strings.Unbounded;
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
      return AUnit.Format ("Main (-h)");
   end Name;

   procedure Run_Test (T : in out Test_1) is
      pragma Unreferenced (T);
      Success   : Boolean;
      Command   : constant String := Command_Path;
      Cmd_Line  : constant String := Command_Line (Command, "-h");
      Arguments : constant Argument_List_Access := Argument_List (Cmd_Line);
   begin
      Spawn (Command, Arguments.all, Success);
      Assert (Success, "[" & Cmd_Line & "] failed");
   end Run_Test;

   --  Private  ---------------------------------------------------------------
   --  See <http://www.adacore.com/2008/11/24/gem-54/>

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

   function Argument_List (Executable_Path : in String;
                          Argument_String : in String)
      return Argument_List_Access
   is
   begin
      return Argument_String_To_List (Executable_Path & " " & Argument_String);
   end Argument_List;

   function Argument_List (Command_Line   : in String)
      return Argument_List_Access
   is
   begin
      return Argument_String_To_List (Command_Line);
   end Argument_List;

end Test_Suite.Main;
