--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Command_Line;
with GNAT.Command_Line;
with Util.IO;
with AdaSpec;
with AdaSpec.CLI;
with AdaSpec.Lang;
with AdaSpec.Job;
with AdaSpec.Generator;

use Ada.Text_IO;
use Ada.Strings.Unbounded;
use Ada.Exceptions;
use Ada.Command_Line;
use GNAT.Command_Line;
use Util.IO;
use AdaSpec;
use AdaSpec.Lang;
use AdaSpec.Job;
use AdaSpec.Generator;

procedure Main is

   use Generator_Vectors;

   Logger     : Logger_Ptr := Logger_Ptr (New_Standard_Logger);
   Env        : Job_Environment;
   Job        : Job_Type;
   Quit       : Boolean := False;
   Options    : constant String := "help h -help k -keep-going " &
              "s: -step= o: -output= x: -executable= l: -lang= " &
              "-fill-steps -progress -partial";
   Arg        : Unbounded_String;
   Step_Dir   : Unbounded_String;
   Out_Dir    : Unbounded_String;
   Language   : Language_Type := Language_Type'First;
   Executable : Unbounded_String;
   Keep_Going : Boolean := False;
   Fill_Steps : Boolean := False;
   Generators : Generator_Vectors.Vector;
   Generator  : Generator_Ptr;
   Progress   : Boolean := False;
   Partial    : Boolean := False;
   I          : Natural;
   Args       : array (1 .. Argument_Count + 1) of Unbounded_String;
   Args_Last  : Natural := 0;

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

      elsif Full_Switch = "-fill-steps" then
         Fill_Steps := True;

      elsif Full_Switch = "-progress" then
         Progress := True;

      elsif Full_Switch = "-partial" then
         Partial := True;

      elsif Full_Switch = "x" or
         Full_Switch = "-executable"
      then
         Executable := To_Unbounded_String (Parameter);

      elsif Full_Switch = "s" or Full_Switch = "-step" then
         if Length (Step_Dir) /= 0 then
            raise Not_Yet_Implemented with "multiple --step";
         end if;
         Step_Dir := To_Unbounded_String (Parameter);

      elsif Full_Switch = "o" or Full_Switch = "-output" then
         Out_Dir  := To_Unbounded_String (Parameter);

      elsif Full_Switch = "l" or Full_Switch = "-lang" then
         Language := Get_Language (Parameter);

      else  --  Never happen unless a bug in Getopt     --  GCOV_IGNORE
         raise Invalid_Switch;                          --  GCOV_IGNORE

      end if;

   end loop Getopt_Loop;

   ----------------------
   --  Get Parameters  --
   ----------------------

   if not Quit then

      Arg := To_Unbounded_String (Get_Argument);
      if Length (Arg) = 0 then
         if Fill_Steps and Length (Step_Dir) /= 0 then
            Put_Line ("--> Fill steps in: " & To_String (Step_Dir));
            New_Line;
            Make (Env,
               Step_Dir => To_String (Step_Dir),
               Out_Dir  => To_String (Out_Dir),
               Language => Language);
            Load (Env, Logger,
               Fill_Steps => True);
         else
            Put_Line (Standard_Error, "Missing feature filename");
            AdaSpec.CLI.Help;
            Set_Exit_Status (Failure);
         end if;
         Quit := True;
      end if;

   end if;

   while not Quit and Length (Arg) > 0 loop
      Args_Last := Args_Last + 1;
      Args (Args_Last) := Arg;
      Arg := To_Unbounded_String (Get_Argument);
   end loop;

   I   := 1;
   Arg := Args (I);
   while not Quit and Length (Arg) > 0 loop

      Put_Line ("--> Compile: " & To_String (Arg));
      New_Line;

      ---------------------
      --  Get Parameter  --
      ---------------------

      Make (Env,
         Step_Dir => To_String (Step_Dir),
         Out_Dir  => To_String (Out_Dir),
         Language => Language);

      Make (Job,
         Feature_File => To_String (Arg));

      Fill_Missing (Env, Feature_File (Job));
      Load (Env, Logger, Fill_Steps);

      ------------------------
      --  Compile Features  --
      ------------------------

      Run (Job, Env, Logger);
      if Job.Result.Fail then
         Put_Line (Standard_Error, "Failure to compile " & Feature_File (Job));
         Set_Exit_Status (Failure);
         if not Keep_Going then
            Quit := True;
         end if;
      elsif not Partial then
         Generate (Job, Env, Logger, Generator);
         Append (Generators, Generator);
      end if;

      Arg := Args (I + 1);

      -------------------
      --  Cleanup Job  --
      -------------------

      Cleanup (Job);

      -------------------

      if Progress then
         Put_Line ("completed" & I'Img & " out of" & Args_Last'Img);
      end if;

      I := I + 1;
      New_Line;

   end loop;

   if not Quit and Executable /= Null_Unbounded_String and not Partial then

      Put_Line ("--> Generate Test Suite: " & To_String (Executable));
      New_Line;

      Generate_Suite (Generators, To_String (Executable), Env, Logger);

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

   Free (Logger);

exception

   when Invalid_Switch =>
      Free (Logger);
      Put_Line (Standard_Error,
                "Invalid switch: -" & Full_Switch & " " & Parameter);
      AdaSpec.CLI.Help;
      Set_Exit_Status (Failure);

   when Invalid_Parameter =>
      Free (Logger);
      Put_Line (Standard_Error, "Missing parameter for switch -" &
                Full_Switch);
      AdaSpec.CLI.Help;
      Set_Exit_Status (Failure);

   when Invalid_Language =>
      Free (Logger);
      Put_Line (Standard_Error, "Unknown language " & Parameter);
      AdaSpec.CLI.Help;
      Set_Exit_Status (Failure);

   when Error : Not_Yet_Implemented =>
      Free (Logger);
      Put_Line (Standard_Error, "Not Yet Implemented: " &
                Exception_Message (Error));
      Set_Exit_Status (Failure);

end Main;
