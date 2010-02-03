--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Command_Line;
with GNAT.Command_Line;
with AUnit.Reporter;
with AUnit.Reporter.Text;
with AUnit.Reporter.XML;
with AUnit.Run;
with Suite;

use Ada.Text_IO;
use Ada.Command_Line;
use GNAT.Command_Line;

procedure Test_Main is

   type Report is (Text, XML);

   Report_Type : Report := Text;

   --  AUnit
   procedure Runner is new AUnit.Run.Test_Runner (Suite.Suite);
   Reporter : access AUnit.Reporter.Reporter'Class;

   --  TODO: we should only have one variable here, but I don't know how to
   --  make an object Xml.Reporter or Text.Reporter in the code without using
   --  the heap (overkill). In C I would use unions, and in Ada I thought I
   --  could use a 'Class type but that doesn't work.

   Reporter_Text : aliased AUnit.Reporter.Text.Text_Reporter;
   Reporter_XML  : aliased AUnit.Reporter.XML.XML_Reporter;

   --  CLI
   procedure Display_Help;
   procedure Display_Help is
   begin
      Put_Line ("tests - run adaspec2 test suite");
      Put_Line ("");
      Put_Line ("SYNOPSIS");
      Put_Line ("");
      Put_Line ("    tests [-help]");
      Put_Line ("    tests [-xml] [-text]");
      Put_Line ("");
      Put_Line ("-- ");
      Put_Line ("Copyright (c) 2010 SOGILIS");
   end Display_Help;

begin

   --  Command Line

   while Getopt ("help h -help xml text") /= ASCII.NUL loop

      if Full_Switch = "h" or
         Full_Switch = "help" or
         Full_Switch = "-help"
      then
         Display_Help;
         return;

      elsif Full_Switch = "xml" then
         Report_Type := XML;

      elsif Full_Switch = "text" then
         Report_Type := Text;

      end if;
   end loop;


   --  AUnit

   case Report_Type is
      when Text =>
         Reporter := Reporter_Text'Access;
      when XML =>
         Reporter := Reporter_XML'Access;
   end case;

   Runner (Reporter.all);

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

end Test_Main;
