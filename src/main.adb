--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Command_Line;
with GNAT.Command_Line;
with Util.Strings.Pool;
with AdaSpec;
with AdaSpec.CLI;
with AdaSpec.Job;
with AdaSpec.Generator.Ada;

use Ada.Text_IO;
use Ada.Strings.Unbounded;
use Ada.Exceptions;
use Ada.Command_Line;
use GNAT.Command_Line;
use Util.Strings.Pool;
use AdaSpec;
use AdaSpec.Job;
use AdaSpec.Generator.Ada;

procedure Main is

   Env       : Job_Environment;
   Job       : Job_Type;
   Quit      : Boolean := False;
   Options   : constant String := "help h -help " &
             "s: -step= o: -output= l: -lang=";
   Buffer    : Unbounded_String;
   Pool      : String_Pool;

begin

   -------------------
   --  Get Options  --
   -------------------

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
         if Length (Env.Step_Dir) /= 0 then
            raise Not_Yet_Implemented with "multiple --step";
         end if;
         Env.Step_Dir := To_Unbounded_String (Parameter);
         --  Put_Line ("--step=" & Parameter);

      elsif Full_Switch = "o" or Full_Switch = "-output" then
         Env.Out_Dir  := To_Unbounded_String (Parameter);
         --  Put_Line ("--output=" & Parameter);

      elsif Full_Switch = "l" or Full_Switch = "-lang" then
         raise Not_Yet_Implemented with "--lang";

      --  Never happen unless a bug in Getopt   --  GCOV_IGNORE
      else                                      --  GCOV_IGNORE
         raise Invalid_Switch;                  --  GCOV_IGNORE

      end if;

   end loop Getopt_Loop;

   ----------------------
   --  Get Parameters  --
   ----------------------

   if not Quit then

      declare
         Arg : constant String := Get_Argument;
      begin
         Make (Job, Arg);
         if Arg'Length = 0 then
            Put_Line (Standard_Error, "Missing feature filename");
            AdaSpec.CLI.Help;
            Set_Exit_Status (Failure);
            Quit := True;
         end if;
      end;

   end if;

   ------------------------
   --  Convert Features  --
   ------------------------

   if not Quit then

      if Get_Argument'Length /= 0 then
         raise Not_Yet_Implemented with "more FEATUREs";
      end if;

      Fill_Missing (Env, Feature_File (Job));
      Load (Env);
      Put_Line (Describe (Job, Env));
      Run (Job, Env);
      Generate_Feature (Buffer, Pool, Job.Result);
      Put_Line (To_String (Buffer));

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
