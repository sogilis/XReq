--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Command_Line;
with GNAT.IO;
with GNAT.Command_Line;
with AUnit.Run;
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

   Quit : Boolean := False;


   --  AUnit  -----------------------------------------------------------------

   procedure Runner_Test     is new AUnit.Run.Test_Runner (Test_Suite.Suite);
   procedure Runner_Coverage is new
      AUnit.Run.Test_Runner (Coverage_Suite.Suite);

   Reporter : AUnit_Reporter.Reporter;

   --  CLI  -------------------------------------------------------------------

   procedure Display_Help;
   procedure Display_Help is
   begin
      Put_Line ("tests - run adaspec2 test suite");
      Put_Line ("");
      Put_Line ("SYNOPSIS");
      Put_Line ("");
      Put_Line ("    tests [-help]");
      Put_Line ("    tests [-suite SUITE] [-xml] [-text]");
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
   while Getopt ("help h -help xml text suite=") /= ASCII.NUL loop

      if Full_Switch = "h" or
         Full_Switch = "help" or
         Full_Switch = "-help"
      then
         Display_Help;
         Quit := True;
         exit Getopt_Loop;

      elsif Full_Switch = "xml" then
         Reporter.Reporter := AUnit_Reporter.Reporter_XML'Access;

      elsif Full_Switch = "text" then
         Reporter.Reporter := AUnit_Reporter.Reporter_Text'Access;

      elsif Full_Switch = "suite" then
         begin
            Choosen_Suite := Suite'Value (Parameter);
         exception
            when Constraint_Error =>
               raise Suite_Error;
         end;

      end if;
   end loop Getopt_Loop;

   if not Quit then

      --  Change output streams so the test output are to stderr and the report
      --  to stdout

      Ada.Text_IO.Set_Output (Ada.Text_IO.Current_Error);
      GNAT.IO    .Set_Output (GNAT.IO    .Standard_Error);

      Reporter.GNAT_IO := AUnit_Reporter.GNAT_IO_out'Access;


      --  AUnit

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
