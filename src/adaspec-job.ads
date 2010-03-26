--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Util.IO;
with AdaSpec.Lang;
with AdaSpec.Features;
with AdaSpec.Steps;
with AdaSpec.Result;

use Ada.Strings.Unbounded;
use Util.IO;
use AdaSpec.Lang;
use AdaSpec.Features;
use AdaSpec.Steps;
use AdaSpec.Result;

package AdaSpec.Job is

   -----------------------
   --  Job_Environment  --
   -----------------------

   Invalid_Environment : exception;

   type Job_Environment is
      record
         Step_Dir  : Unbounded_String;
         Out_Dir   : Unbounded_String;
         Steps     : Steps_Type;
         Loaded    : Boolean := False;
         Language  : Language_Type := Lang_Ada;
      end record;
   Null_Job_Environment : constant Job_Environment := (others => <>);

   procedure Make         (Env        : out    Job_Environment;
                           Step_Dir   : in     String := "";
                           Out_Dir    : in     String := "";
                           Language   : in     Language_Type := Lang_Ada);
   function  Step_Dir     (Env        : in     Job_Environment) return String;
   function  Out_Dir      (Env        : in     Job_Environment) return String;
   procedure Fill_Missing (Env        : in out Job_Environment;
                           Feature    : in     String);
   procedure Load         (Env        : in out Job_Environment;
                           Logger     : in     Logger_Ptr;
                           Fill_Steps : in     Boolean := False);
   --  IMPORTANT: don't forget to call UnLoad
   procedure UnLoad       (Env      : in out Job_Environment);

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

   procedure Make     (Job           : out    Job_Type;
                       Feature_File  : in     String);
   procedure Run      (Job           : in out Job_Type;
                       Env           : in     Job_Environment;
                       Logger        : in     Logger_Ptr;
                       Step_Matching : in     Boolean := False);
   --  IMPORTANT: call Cleanup afterwards
   procedure Cleanup  (Job           : in out Job_Type);
   function  Describe (Job           : in     Job_Type;
                       Env           : in     Job_Environment) return String;


   procedure Init (Env          : out    Job_Environment;
                   Job          : out    Job_Type;
                   Logger       : in     Logger_Ptr;
                   Feature_File : in     String;
                   Step_Dir     : in     String := "";
                   Out_Dir      : in     String := "");
   --  IMPORTANT: run UnLoad in Env

end AdaSpec.Job;
