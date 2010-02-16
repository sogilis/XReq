--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with AdaSpec.Features;
with AdaSpec.Steps;
with AdaSpec.Result;

use Ada.Strings.Unbounded;
use AdaSpec.Features;
use AdaSpec.Steps;
use AdaSpec.Result;

package AdaSpec.Job is

   -----------------------
   --  Job_Environment  --
   -----------------------

   type Job_Environment is
      record
         Step_Dir  : Unbounded_String;
         Out_Dir   : Unbounded_String;
         Steps     : Step_Vectors.Vector;
      end record;

   procedure Make         (Env      : out    Job_Environment;
                           Step_Dir : in     String := "";
                           Out_Dir  : in     String := "");
   function  Step_Dir     (Env      : in     Job_Environment) return String;
   function  Out_Dir      (Env      : in     Job_Environment) return String;
   procedure Fill_Missing (Env      : in out Job_Environment;
                           Feature  : in     String);

   ----------------
   --  Job_Type  --
   ----------------

   --  This type describes a job to run, that is a feature that is to be
   --  compiled in a test file.

   type Job_Type is
      record
         Feature_File : Unbounded_String;
         Feature      : Feature_Ptr;
         Result       : Result_Feature_Type;
      end record;

   function  Feature_File (Job : in Job_Type) return String;

   procedure Make     (Job          : out    Job_Type;
                       Feature_File : in     String);
   procedure Run      (Job          : in out Job_Type;
                       Env          : in     Job_Environment);
   function  Describe (Job          : in     Job_Type;
                       Env          : in     Job_Environment) return String;

end AdaSpec.Job;
