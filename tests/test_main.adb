--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Command_Line;
with GNAT.Command_Line;
with AUnit.Reporter;
with AUnit.Reporter.Text;
with AUnit.Reporter.XML;
with AUnit.Run;
with Test_Suite;
with Coverage_Suite;

use Ada.Text_IO;
use Ada.Command_Line;
use GNAT.Command_Line;

procedure Test_Main is

   type Suite is (Test, Coverage);
   Choosen_Suite : Suite := Test;
   Suite_Error   : exception;

   --  AUnit  -----------------------------------------------------------------

   --  TODO: we should only have one variable here, but I don't know how to
   --  make an object Xml.Reporter or Text.Reporter in the code without using
   --  the heap (overkill). In C I would use unions, and in Ada I thought I
   --  could use a 'Class type but that doesn't work.

   procedure Runner_Test     is new AUnit.Run.Test_Runner (Test_Suite.Suite);
   procedure Runner_Coverage is new
      AUnit.Run.Test_Runner (Coverage_Suite.Suite);

   Reporter_Text : aliased AUnit.Reporter.Text.Text_Reporter;
   Reporter_XML  : aliased AUnit.Reporter.XML.XML_Reporter;
   Reporter : access AUnit.Reporter.Reporter'Class := Reporter_Text'Access;

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

   while Getopt ("help h -help xml text suite=") /= ASCII.NUL loop

      if Full_Switch = "h" or
         Full_Switch = "help" or
         Full_Switch = "-help"
      then
         Display_Help;
         return;

      elsif Full_Switch = "xml" then
         Reporter := Reporter_XML'Access;

      elsif Full_Switch = "text" then
         Reporter := Reporter_Text'Access;

      elsif Full_Switch = "suite" then
         begin
            Choosen_Suite := Suite'Value (Parameter);
         exception
            when Constraint_Error =>
               raise Suite_Error;
         end;

      end if;
   end loop;


   --  AUnit

   case Choosen_Suite is
      when Test =>
         Runner_Test (Reporter.all);
      when Coverage =>
         Runner_Coverage (Reporter.all);
--       when others =>
--          Put_Line (Standard_Error, "Suite " & Suite'Image (Choosen_Suite) &
--                    " not implemented");
--          Set_Exit_Status (Failure);
   end case;

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
