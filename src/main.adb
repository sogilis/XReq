--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Command_Line;
with GNAT.Command_Line;
with AdaSpec;
with AdaSpec.CLI;
with AdaSpec.Job;

use Ada.Text_IO;
use Ada.Strings.Unbounded;
use Ada.Exceptions;
use Ada.Command_Line;
use GNAT.Command_Line;
use AdaSpec;
use AdaSpec.Job;

procedure Main is

   Job       : Job_Type;
   Quit      : Boolean := False;
   Options   : constant String := "help h -help " &
             "s: -step= o: -output= l: -lang=";

begin

   Getopt_Loop :
   while Getopt (Options) /= ASCII.NUL loop

      if Full_Switch = "h" or
         Full_Switch = "help" or
         Full_Switch = "-help"
      then
         AdaSpec.CLI.Help;
         Quit := True;
         exit Getopt_Loop;

      elsif Full_Switch = "s" or Full_Switch = "-step" then
         if Length (Job.Step_Dir) /= 0 then
            raise Not_Yet_Implemented with "multiple --step";
         end if;
         Job.Step_Dir := To_Unbounded_String (Parameter);
         --  Put_Line ("--step=" & Parameter);

      elsif Full_Switch = "o" or Full_Switch = "-output" then
         Job.Out_Dir  := To_Unbounded_String (Parameter);
         --  Put_Line ("--output=" & Parameter);

      elsif Full_Switch = "l" or Full_Switch = "-lang" then
         raise Not_Yet_Implemented with "--lang";

--       else
--          raise Invalid_Switch;

      end if;

   end loop Getopt_Loop;

   if not Quit then

      declare
         Arg : constant String := Get_Argument;
      begin
         Job.Feature := To_Unbounded_String (Arg);
         if Arg'Length = 0 then
            Put_Line (Standard_Error, "Missing feature filename");
            AdaSpec.CLI.Help;
            Set_Exit_Status (Failure);
            Quit := True;
         end if;
      end;

   end if;

   if not Quit then

      if Get_Argument'Length /= 0 then
         raise Not_Yet_Implemented with "more FEATUREs";
      end if;

      Fill_Missing (Job);

      Put_Line (Describe (Job));

   end if;

exception

   when Invalid_Switch =>
      Put_Line (Standard_Error,
                "Invalid switch: -" & Full_Switch & " " & Parameter);
      AdaSpec.CLI.Help;
      Set_Exit_Status (Failure);

   when Invalid_Parameter =>
      Put_Line (Standard_Error, "Missing parameter for switch -" &
                Full_Switch);
      AdaSpec.CLI.Help;
      Set_Exit_Status (Failure);

   when Error : Not_Yet_Implemented =>
      Put_Line (Standard_Error, "Not Yet Implemented: " &
                Exception_Message (Error));
      Set_Exit_Status (Failure);

end Main;
