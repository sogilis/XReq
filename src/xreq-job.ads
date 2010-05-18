--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Hashed_Maps;
with Util.IO;
with XReq.Lang;
with XReq.Features;
with XReq.Step_Definitions;
with XReq.Result;
with XReqLib;

use Ada.Strings.Unbounded;
use Util.IO;
use XReq.Lang;
use XReq.Features;
use XReq.Step_Definitions;
use XReq.Result;
use XReqLib;

package XReq.Job is

   -----------------------
   --  Job_Environment  --
   -----------------------

   package Options_Pkg is new Ada.Containers.Hashed_Maps
     (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type    => Ada.Strings.Unbounded.Unbounded_String,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded."=",
      "="             => Ada.Strings.Unbounded."=");

   Invalid_Environment : exception;
   Invalid_Option      : exception;

   type Job_Environment is
      record
         Step_Dir  : String_Vector;
         Out_Dir   : Unbounded_String;
         Steps     : Step_Definitions_Type;
         Loaded    : Boolean := False;
         Language  : Language_Type := Lang_Ada;
         Options   : Options_Pkg.Map;
      end record;
   Null_Job_Environment : constant Job_Environment := (others => <>);

   procedure Make         (Env        : out    Job_Environment;
                           Step_Dir   : in     String_Vector :=
                                               Empty_String_Vector;
                           Out_Dir    : in     String := "";
                           Language   : in     Language_Type := Lang_Ada);
   procedure Make         (Env        : out    Job_Environment;
                           Step_Dir   : in     String;
                           Out_Dir    : in     String := "";
                           Language   : in     Language_Type := Lang_Ada);
   function  First_Step_Dir (Env      : in     Job_Environment) return String;
   function  Out_Dir      (Env        : in     Job_Environment) return String;
   procedure Fill_Missing (Env        : in out Job_Environment;
                           Feature    : in     String);
   procedure Load         (Env        : in out Job_Environment;
                           Logger     : in     Logger_Ptr;
                           Fill_Steps : in     Boolean := False);
   procedure Set_Option   (Env        : in out Job_Environment;
                           Name       : in     String;
                           Value      : in     String);
   function  Get_Option   (Env        : in     Job_Environment;
                           Name       : in     String) return String;
   function  Get_Option   (Env        : in     Job_Environment;
                           Name       : in     String;
                           Default    : in     String) return String;
   function  Has_Option   (Env        : in     Job_Environment;
                           Name       : in     String) return Boolean;
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
                       Env           : in out Job_Environment;
                       Logger        : in     Logger_Ptr;
                       Add_Steps_Pkg : in     String  := "";
                       Step_Matching : in     Boolean := False);
   --  IMPORTANT: call Cleanup afterwards
   procedure Cleanup  (Job           : in out Job_Type);


   procedure Init (Env          : out    Job_Environment;
                   Job          : out    Job_Type;
                   Logger       : in     Logger_Ptr;
                   Feature_File : in     String;
                   Step_Dir     : in     String_Vector :=
                                         Empty_String_Vector;
                   Out_Dir      : in     String := "");
   --  IMPORTANT: run UnLoad in Env

end XReq.Job;
