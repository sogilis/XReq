--                         Copyright (C) 2010, Sogilis                       --

with Ada.Directories;

use Ada.Directories;

package body AdaSpec.Job is

   ---------------------------------
   --  Job_Environment  -- Make   --
   ---------------------------------

   procedure Make         (Env      : out    Job_Environment;
                           Step_Dir : in     String := "";
                           Out_Dir  : in     String := "") is
   begin
      Env := (
         Step_Dir => To_Unbounded_String (Step_Dir),
         Out_Dir  => To_Unbounded_String (Out_Dir),
         others   => <>);
   end Make;

   -------------------------------------
   --  Job_Environment  --  Step_Dir  --
   -------------------------------------

   function  Step_Dir     (Env      : in     Job_Environment) return String is
   begin
      return To_String (Env.Step_Dir);
   end Step_Dir;

   ------------------------------------
   --  Job_Environment  --  Out_Dir  --
   ------------------------------------

   function  Out_Dir      (Env      : in     Job_Environment) return String is
   begin
      return To_String (Env.Out_Dir);
   end Out_Dir;

   -----------------------------------------
   --  Job_Environment  --  Fill_Missing  --
   -----------------------------------------

   procedure Fill_Missing (Env : in out Job_Environment;
                           Feature : in String) is
   begin

      if Length (Env.Step_Dir) = 0 then
         Env.Step_Dir := To_Unbounded_String (Compose (
            Containing_Directory (Feature), "step_definitions"));
      end if;

      if Length (Env.Out_Dir) = 0 then
         Env.Out_Dir  := To_Unbounded_String (Compose (
            Containing_Directory (Feature), "tests"));
      end if;

   end Fill_Missing;

   ---------------------------------
   --  Job_Environment  --  Load  --
   ---------------------------------

   procedure Load (Env : in out Job_Environment)
   is
   begin

      if Length (Env.Step_Dir) = 0 then
         raise Invalid_Environment with "No step_definitions directory";
      end if;
      if Length (Env.Out_Dir) = 0 then
         raise Invalid_Environment with "No output directory";
      end if;

      Create_Path (Step_Dir (Env));
      Create_Path (Out_Dir (Env));

      Load (Env.Steps, Step_Dir (Env));
      Env.Loaded := True;

   end Load;

   --------------------------
   --  Job_Type  --  Make  --
   --------------------------

   procedure Make (Job          : out    Job_Type;
                   Feature_File : in     String) is
   begin
      Job := (
         Feature_File => To_Unbounded_String (Feature_File),
         others   => <>);
   end Make;

   ----------------------------------
   --  Job_Type  --  Feature_File  --
   ----------------------------------

   function  Feature_File (Job : in Job_Type) return String is begin
      return To_String (Job.Feature_File);
   end Feature_File;

   ------------------------------
   --  Job_Type  --  Describe  --
   ------------------------------

   function Describe (Job : in Job_Type;
                      Env : in Job_Environment) return String is
      CRLF : constant String := ASCII.CR & ASCII.LF;
   begin
      return "Feature:     " & Feature_File (Job) & CRLF &
             "Steps in:    " & Step_Dir (Env)     & CRLF &
             "Generate in: " & Out_Dir (Env)      & CRLF;
   end Describe;

   -------------------------
   --  Job_Type  --  Run  --
   -------------------------

   procedure Run (Job : in out Job_Type;
                  Env : in     Job_Environment)
   is
      F : constant Feature_File_Ptr := new Feature_File_Type;
   begin
      if not Env.Loaded then
         raise Invalid_Environment with "Must call Load (Env) first";
      end if;

      Make (F.all, Feature_File (Job));
      Parse (F.all);
      Job.Feature := Feature_Ptr (F);

      Process_Feature (Job.Result, Job.Feature, Env.Steps);
   end Run;


end AdaSpec.Job;
