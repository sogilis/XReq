--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Command_Line;
with GNAT.Command_Line;
with AdaSpec;
with AdaSpec.CLI;
with AdaSpec.Job;
with AdaSpec.Generator;

use Ada.Text_IO;
use Ada.Strings.Unbounded;
use Ada.Exceptions;
use Ada.Command_Line;
use GNAT.Command_Line;
use AdaSpec;
use AdaSpec.Job;
use AdaSpec.Generator;

procedure Main is

   use Generator_Vectors;

   Env        : Job_Environment;
   Job        : Job_Type;
   Quit       : Boolean := False;
   Options    : constant String := "help h -help k -keep-going " &
              "s: -step= o: -output= x: -executable= l: -lang=";
   Arg        : Unbounded_String;
   Step_Dir   : Unbounded_String;
   Out_Dir    : Unbounded_String;
   Executable : Unbounded_String;
   Keep_Going : Boolean := False;
   Generators : Generator_Vectors.Vector;
   Generator  : Generator_Ptr;

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

      elsif Full_Switch = "k" or
         Full_Switch = "-keep-going"
      then
         Keep_Going := True;

      elsif Full_Switch = "x" or
         Full_Switch = "-executable"
      then
         Executable := To_Unbounded_String (Parameter);

      elsif Full_Switch = "s" or Full_Switch = "-step" then
         if Length (Step_Dir) /= 0 then
            raise Not_Yet_Implemented with "multiple --step";
         end if;
         Step_Dir := To_Unbounded_String (Parameter);
         --  Put_Line ("--step=" & Parameter);

      elsif Full_Switch = "o" or Full_Switch = "-output" then
         Out_Dir  := To_Unbounded_String (Parameter);
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

      Arg := To_Unbounded_String (Get_Argument);
      if Length (Arg) = 0 then
         Put_Line (Standard_Error, "Missing feature filename");
         AdaSpec.CLI.Help;
         Set_Exit_Status (Failure);
         Quit := True;
      end if;

   end if;

   while not Quit and Length (Arg) > 0 loop

      ---------------------
      --  Get Parameter  --
      ---------------------

      Make (Env, To_String (Step_Dir), To_String (Out_Dir));
      Make (Job, To_String (Arg));
      Fill_Missing (Env, Feature_File (Job));
      Load (Env);

      Put_Line (Describe (Job, Env));

      ------------------------
      --  Compile Features  --
      ------------------------

      Run (Job, Env);
      if Job.Result.Fail then
         Put_Line (Standard_Error, "Failure to compile " & Feature_File (Job));
         Set_Exit_Status (Failure);
         if not Keep_Going then
            Quit := True;
         end if;
      else
         Generate (Job, Env, Generator);
         Append (Generators, Generator);
      end if;

      Arg := To_Unbounded_String (Get_Argument);

      -------------------
      --  Cleanup Job  --
      -------------------

      Cleanup (Job);

   end loop;

   if not Quit and Executable /= Null_Unbounded_String then
      Generate_Suite (Generators, To_String (Executable), Env);
   end if;

   -------------------
   --  Free memory  --
   -------------------

   UnLoad (Env);

   declare
      I : Generator_Vectors.Cursor := First (Generators);
      E : Generator_Ptr;
   begin
      while Has_Element (I) loop
         E := Element (I);
         Free (E);
         Next (I);
      end loop;
      Clear (Generators);
   end;

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
