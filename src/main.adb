--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Command_Line;
with GNAT.Command_Line;
with Util.IO;
with XReqLib;
with XReq;
with XReq.CLI;
with XReq.Lang;
with XReq.Job;
with XReq.Generator;

use Ada.Text_IO;
use Ada.Strings.Unbounded;
use Ada.Exceptions;
use Ada.Command_Line;
use GNAT.Command_Line;
use Util.IO;
use XReqLib;
use XReq;
use XReq.Lang;
use XReq.Job;
use XReq.Generator;

procedure Main is

   use Generator_Vectors;
   use String_Vectors;

   Logger     : Logger_Ptr := Logger_Ptr (New_Standard_Logger);
   Logger2    : Logger_Ptr := Logger;
   Env        : Job_Environment;
   Job        : Job_Type;
   Quit       : Boolean := False;
   Options    : constant String := "help h -help k -keep-going " &
              "s: -step= o: -output= x: -executable= l: -lang= " &
              "-fill-steps -progress -partial -step-matching m -make " &
              "q -quiet -fill-steps-in=";
   Arg        : Unbounded_String;
   Step_Dir   : String_Vector;
   Out_Dir    : Unbounded_String;
   Language   : Language_Type := Language_Type'First;
   Executable : Unbounded_String;
   Keep_Going : Boolean := False;
   Fill_Steps : Boolean := False;
   Generators : Generator_Vectors.Vector;
   Generator  : Generator_Ptr;
   Progress   : Boolean := False;
   Partial    : Boolean := False;
   Step_Match : Boolean := False;
   Make       : Boolean := False;
   Fill_Pkg   : Unbounded_String;
   I          : Natural;
   J          : String_Vectors.Cursor;
   Args       : array (1 .. Argument_Count + 1) of Unbounded_String;
   Args_Last  : Natural := 0;
   XREQ_BEFORE_MAKE : constant String := GetEnv ("XREQ_BEFORE_MAKE", "");
   XREQ_BEFORE_MAKE_SILENT : constant String
              := GetEnv ("XREQ_BEFORE_MAKE_SILENT", "");

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
         XReq.CLI.Help;
         Quit := True;
         exit Getopt_Loop;

      elsif Full_Switch = "k" or
         Full_Switch = "-keep-going"
      then
         Keep_Going := True;

      elsif Full_Switch = "m" or
         Full_Switch = "-make"
      then
         Make := not Partial;

      elsif Full_Switch = "q" or
         Full_Switch = "-quiet"
      then
         Logger.Set_Verbosity (-1);

      elsif Full_Switch = "-fill-steps-in" then
         Fill_Steps := True;
         Fill_Pkg   := To_Unbounded_String (Parameter);

      elsif Full_Switch = "-fill-steps" then
         Fill_Steps := True;

      elsif Full_Switch = "-progress" then
         Progress := True;

      elsif Full_Switch = "-partial" then
         Partial := True;
         Make    := False;

      elsif Full_Switch = "-step-matching" then
         Step_Match := True;

      elsif Full_Switch = "x" or
         Full_Switch = "-executable"
      then
         Executable := To_Unbounded_String (Parameter);

      elsif Full_Switch = "s" or Full_Switch = "-step" then
         Append (Step_Dir, To_Unbounded_String (Parameter));

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
         if Fill_Steps and not Is_Empty (Step_Dir) then
            J := First (Step_Dir);
            while Has_Element (J) loop
               Put_Line ("--> Fill steps in: " & To_String (Element (J)));
               Next (J);
            end loop;
            New_Line;
            XReq.Job.Make (Env,
               Step_Dir => Step_Dir,
               Out_Dir  => To_String (Out_Dir),
               Language => Language);
            Load (Env, Logger,
               Fill_Steps => True);
         else
            Put_Line (Standard_Error, "Missing feature filename");
            XReq.CLI.Help;
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

      Logger.Put_Line ("--> Compile: " & To_String (Arg));
      Logger.New_Line;

      ---------------------
      --  Get Parameter  --
      ---------------------

      XReq.Job.Make (Env,
         Step_Dir => Step_Dir,
         Out_Dir  => To_String (Out_Dir),
         Language => Language);

      XReq.Job.Make (Job,
         Feature_File => To_String (Arg));

      Fill_Missing (Env, Feature_File (Job));
      Load (Env, Logger, Fill_Steps);

      ------------------------
      --  Compile Features  --
      ------------------------

      Run (Job, Env, Logger, To_String (Fill_Pkg), Step_Match);
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
         Logger.Put_Line (-1, "completed" & I'Img & " out of" & Args_Last'Img);
      end if;

      I := I + 1;
      if not Quit and Length (Arg) > 0 then
         Logger.New_Line;
      end if;

   end loop;

   if XREQ_BEFORE_MAKE /= "" then
      if XREQ_BEFORE_MAKE_SILENT /= "" then
         Logger2 := Null_Logger;
      end if;
      Logger2.New_Line;
      Logger2.Put_Line ("--> Execute XREQ_BEFORE_MAKE: " & XREQ_BEFORE_MAKE);
      declare
         Buffer  : Unbounded_String;
         Status  : Integer;
      begin
         System (XREQ_BEFORE_MAKE, Buffer, Status);
         Logger2.Put (Buffer);
         if Status = 0 then     Logger2.Put_Line ("--> Success");
         else                   Logger2.Put_Line ("--> Failure:" & Status'Img);
         end if;
      end;
      Logger2.New_Line;
   end if;
   Logger2 := Logger;

   if not Quit and Executable /= Null_Unbounded_String and not Partial then

      Logger.New_Line;
      Logger.Put_Line ("--> Generate Test Suite: " & To_String (Executable));
      Logger.New_Line;

      begin
         Generate_Suite (Generators,
            To_String (Executable), Env, Logger, Make);
      exception                         --  GCOV_IGNORE_BEGIN
         when Generation_Error =>       --  Should never happen unless
            Set_Exit_Status (Failure);  --  gnatmake failed. But that's a bug.
      end;                              --  GCOV_IGNORE_END

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
      XReq.CLI.Help;
      Set_Exit_Status (Failure);

   when Invalid_Parameter =>
      Free (Logger);
      Put_Line (Standard_Error, "Missing parameter for switch -" &
                Full_Switch);
      XReq.CLI.Help;
      Set_Exit_Status (Failure);

   when Invalid_Language =>
      Free (Logger);
      Put_Line (Standard_Error, "Unknown language " & Parameter);
      XReq.CLI.Help;
      Set_Exit_Status (Failure);

   --  GCOV_IGNORE_BEGIN
   when Error : Not_Yet_Implemented =>
      Free (Logger);
      Put_Line (Standard_Error, "Not Yet Implemented: " &
                Exception_Message (Error));
      Set_Exit_Status (Failure);
   --  GCOV_IGNORE_END

end Main;
