--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with AdaSpec.Result;

use Ada.Strings.Unbounded;
use AdaSpec.Result;

package AdaSpec.Job is

   Invalid_Job : exception;

   ----------------
   --  Job_Type  --
   ----------------

   --  This type describes a job to run, that is a feature that is to be
   --  compiled in a test file.

   type Job_Type is record
      Feature   : Unbounded_String;
      Step_Dir  : Unbounded_String;
      Out_Dir   : Unbounded_String;
   end record;

   procedure Fill_Missing (Job : in out Job_Type);

   function Describe (Job : in Job_Type) return String;

   procedure Run (Job    : in  Job_Type;
                  Result : out Result_Feature_Type);

end AdaSpec.Job;
