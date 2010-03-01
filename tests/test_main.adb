--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Command_Line;
with GNAT.Command_Line;
with AUnit.Run;
--  with AUnit.Reporter;
--  with AUnit.Reporter.XML2;
--  with AUnit.Reporter.Text2;
with AUnit_Reporter;
with Test_Suite;
with Coverage_Suite;

use Ada.Text_IO;
use Ada.Command_Line;
use GNAT.Command_Line;

procedure Test_Main is

   type Suite is (Test, Coverage);
   Choosen_Suite : Suite := Test;
   Suite_Error   : exception;

   Quit   : Boolean := False;
--    File   : File_Type;

   --  AUnit  -----------------------------------------------------------------

   procedure Runner_Test     is new AUnit.Run.Test_Runner (Test_Suite.Suite);
   procedure Runner_Coverage is new
      AUnit.Run.Test_Runner (Coverage_Suite.Suite);

--    Reporter_XML    : aliased AUnit.Reporter.XML2.XML_Reporter;
--    Reporter_Text   : aliased AUnit.Reporter.Text2.Text_Reporter;
--    Reporter        : access  AUnit.Reporter.Reporter'Class :=
--                      Reporter_Text'Access;
   Reporter : AUnit_Reporter.Reporter;

   --  CLI  -------------------------------------------------------------------

   procedure Display_Help;
   procedure Display_Help is
   begin
      Put_Line ("tests - run adaspec test suite");
      Put_Line ("");
      Put_Line ("SYNOPSIS");
      Put_Line ("");
      Put_Line ("    tests [-help]");
      Put_Line ("    tests [-suite SUITE] [-xml] [-text] [-o OUTFILE]");
      Put_Line ("");
      Put_Line ("AVAILABLE TEST SUITES");
      Put_Line ("");
      for s in Suite'Range loop
         Put_Line ("    " & Suite'Image (s));
      end loop;
      Put_Line ("");
      Put_Line ("-- ");
      Put_Line ("Copyright (c) 2010 SOGILIS");
   end Display_Help;

begin

   --  Command Line

   Getopt_Loop :
   while Getopt ("help h -help xml text suite= o:") /= ASCII.NUL loop

      if Full_Switch = "h" or
         Full_Switch = "help" or
         Full_Switch = "-help"
      then
         Display_Help;
         Quit := True;
         exit Getopt_Loop;

      elsif Full_Switch = "suite" then
         begin
            Choosen_Suite := Suite'Value (Parameter);
         exception
            when Constraint_Error =>
               raise Suite_Error;
         end;

      elsif Full_Switch = "xml" then
--          Reporter := Reporter_XML'Access;
         Reporter.Reporter := AUnit_Reporter.Reporter_XML'Access;

      elsif Full_Switch = "text" then
--          Reporter := Reporter_Text'Access;
         Reporter.Reporter := AUnit_Reporter.Reporter_Text'Access;

      elsif Full_Switch = "o" then
--          Create (File, Out_File, Parameter);
         Create (AUnit_Reporter.Output_File, Out_File, Parameter);

      end if;
   end loop Getopt_Loop;

   if not Quit then

      --  AUnit

      if Is_Open (AUnit_Reporter.Output_File) then
         Reporter.File := AUnit_Reporter.Output_File'Access;
      else
         AUnit_Reporter.Reporter_Text.Set_Use_ANSI_Colors (True);
      end if;

      case Choosen_Suite is
         when Test =>
            Runner_Test (Reporter);
         when Coverage =>
            Runner_Coverage (Reporter);
--          when others =>
--             Put_Line (Standard_Error, "Suite " &
--                       Suite'Image (Choosen_Suite) & " not implemented");
--             Set_Exit_Status (Failure);
      end case;

   end if;

exception

   when Invalid_Switch =>
      Put_Line (Standard_Error,
         "Invalid switch: " & Full_Switch & " " & Parameter);
      Display_Help;
      Set_Exit_Status (Failure);

   when Invalid_Parameter =>
      Put_Line (Standard_Error, "Missing parameter for switch " & Full_Switch);
      Display_Help;
      Set_Exit_Status (Failure);

   when Suite_Error =>
      Put_Line (Standard_Error, "Incorrect test suite chosen " & Parameter);
      Display_Help;
      Set_Exit_Status (Failure);

end Test_Main;
