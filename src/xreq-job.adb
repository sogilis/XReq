--                         Copyright (C) 2010, Sogilis                       --

with Ada.Directories;
with Ada.Containers;
with Util.Strings;
with XReq.Result_Features;

use Ada.Directories;
use Util.Strings;
use XReq.Result_Features;

package body XReq.Job is

   ---------------------------------
   --  Job_Environment  -- Make   --
   ---------------------------------

   procedure Make         (Env      : out    Job_Environment;
                           Step_Dir : in     String_Vector :=
                                             Empty_String_Vector;
                           Out_Dir  : in     String := "";
                           Language : in     Language_Type := Lang_Ada) is
   begin
      Env := (
         Step_Dir => Step_Dir,
         Out_Dir  => To_Unbounded_String (Out_Dir),
         Language => Language,
         others   => <>);
   end Make;

   ---------------------------------
   --  Job_Environment  -- Make   --
   ---------------------------------

   procedure Make         (Env      : out    Job_Environment;
                           Step_Dir : in     String;
                           Out_Dir  : in     String := "";
                           Language : in     Language_Type := Lang_Ada)
   is
      use String_Vectors;
      V : String_Vector;
   begin
      Append (V, To_Unbounded_String (Step_Dir));
      Make (Env, V, Out_Dir, Language);
   end Make;

   -------------------------------------
   --  Job_Environment  --  Step_Dir  --
   -------------------------------------

   function  First_Step_Dir (Env : in Job_Environment) return String is
      use String_Vectors;
      use Ada.Containers;
   begin
      if Length (Env.Step_Dir) >= 1 then
         return To_String (First_Element (Env.Step_Dir));
      else
         raise Constraint_Error with "No step dir";
      end if;
   end First_Step_Dir;

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
                           Feature : in String)
   is
      use String_Vectors;
   begin

      if Is_Empty (Env.Step_Dir) then
         Append (Env.Step_Dir, To_Unbounded_String (Compose (
            Containing_Directory (Feature), "step_definitions")));
      end if;

      if Length (Env.Out_Dir) = 0 then
         Env.Out_Dir  := To_Unbounded_String (Compose (
            Containing_Directory (Feature), "tests"));
      end if;

   end Fill_Missing;

   ---------------------------------
   --  Job_Environment  --  Load  --
   ---------------------------------

   procedure Load (Env        : in out Job_Environment;
                   Logger     : in     Logger_Ptr;
                   Fill_Steps : in     Boolean := False)
   is
      use String_Vectors;
   begin

      if Is_Empty (Env.Step_Dir) then
         raise Invalid_Environment with "No step_definitions directory";
      end if;
      if Length (Env.Out_Dir) = 0 then
         if not Fill_Steps then
            raise Invalid_Environment with "No output directory";
         end if;
      else
         Create_Path (Out_Dir (Env));
      end if;

      for I in First_Index (Env.Step_Dir) .. Last_Index (Env.Step_Dir) loop
         declare
            Step : constant String := To_String (Element (Env.Step_Dir, I));
         begin
            Create_Path (Step);
            Load (Env.Steps, Logger, Step, Env.Language, Fill_Steps);
         end;
      end loop;

      Env.Loaded := True;

   end Load;

   ---------------------------------------
   --  Job_Environment  --  Set_Option  --
   ---------------------------------------

   procedure Set_Option   (Env        : in out Job_Environment;
                           Name       : in     String;
                           Value      : in     String)
   is
      use Options_Pkg;
   begin
      Include (Env.Options,
               To_Unbounded_String (Name),
               To_Unbounded_String (Value));
   end Set_Option;

   ---------------------------------------
   --  Job_Environment  --  Get_Option  --
   ---------------------------------------

   function  Get_Option   (Env        : in     Job_Environment;
                           Name       : in     String) return String
   is
      use Options_Pkg;
      I : Cursor;
   begin
      I := Find (Env.Options, To_Unbounded_String (Name));
      if Has_Element (I) then
         return To_String (Element (I));
      else
         raise Invalid_Option;
      end if;
   end Get_Option;

   ---------------------------------------
   --  Job_Environment  --  Get_Option  --
   ---------------------------------------

   function  Get_Option   (Env        : in     Job_Environment;
                           Name       : in     String;
                           Default    : in     String) return String
   is
      use Options_Pkg;
      I : Cursor;
   begin
      I := Find (Env.Options, To_Unbounded_String (Name));
      if Has_Element (I) then
         return To_String (Element (I));
      else
         return Default;
      end if;
   end Get_Option;

   ---------------------------------------
   --  Job_Environment  --  Has_Option  --
   ---------------------------------------

   function  Has_Option   (Env        : in     Job_Environment;
                           Name       : in     String) return Boolean
   is
      use Options_Pkg;
   begin
      return Has_Element (Find (Env.Options, To_Unbounded_String (Name)));
   end Has_Option;

   -----------------------------------
   --  Job_Environment  --  UnLoad  --
   -----------------------------------

   procedure UnLoad (Env : in out Job_Environment) is
   begin
      Free (Env.Steps);
      Env := Null_Job_Environment;
   end UnLoad;

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

   -------------------------
   --  Job_Type  --  Run  --
   -------------------------

   procedure Run (Job           : in out Job_Type;
                  Env           : in out Job_Environment;
                  Logger        : in     Logger_Ptr;
                  Add_Steps_Pkg : in     String  := "";
                  Step_Matching : in     Boolean := False)
   is
      use String_Sets;
      F : constant Feature_File_Ptr := new Feature_File_Type;
      Missing_Steps : String_Set;
   begin
      if not Env.Loaded then
         raise Invalid_Environment with "Must call Load (Env) first";
      end if;

      F.Make (Feature_File (Job));
      Job.Feature := Feature_Ptr (F);

      F.Parse (Logger);

      --  No Parse_Error
      Logger.Put_Line ("Compile: " & To_String (Job.Feature_File));
      Job.Result.Process_Feature (Job.Feature, Env.Steps, Logger,
                                  Missing_Steps, Step_Matching);

      if Add_Steps_Pkg /= "" and not Is_Empty (Missing_Steps) then
         Add_Steps (Env.Steps, Missing_Steps, Add_Steps_Pkg,
                    First_Step_Dir (Env), Env.Language, Logger);
         Clear (Missing_Steps);
         Job.Result.Set_Fail (False);
         Job.Result.Process_Feature (Job.Feature, Env.Steps, Logger,
                                     Missing_Steps, Step_Matching);
      end if;
   exception
      when Parse_Error =>
         Job.Result.Set_Fail;
   end Run;

   -----------------------------
   --  Job_Type  --  Cleanup  --
   -----------------------------

   procedure Cleanup (Job : in out Job_Type) is
   begin
      Free (Job.Feature);
   end Cleanup;

   ------------
   --  Init  --
   ------------

   procedure Init (Env          : out    Job_Environment;
                   Job          : out    Job_Type;
                   Logger       : in     Logger_Ptr;
                   Feature_File : in     String;
                   Step_Dir     : in     String_Vector :=
                                         Empty_String_Vector;
                   Out_Dir      : in     String := "")
   is
      E : Job_Environment;
      J : Job_Type;
   begin
      Make (J, Feature_File);
      Make (E, Step_Dir, Out_Dir);
      Fill_Missing (E, Feature_File);
      Load (E, Logger);
      Env := E;
      Job := J;
   end Init;


end XReq.Job;
