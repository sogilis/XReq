--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;

package AdaSpec.Job is

   Invalid_Job : exception;

   type Job_Type is record
      Feature   : Unbounded_String;
      Step_Dir  : Unbounded_String;
      Out_Dir   : Unbounded_String;
   end record;

   type Job_Result_Type is null record;

   procedure Fill_Missing (Job : in out Job_Type);

   function Describe (Job : in Job_Type) return String;

   procedure Run (Job    : in  Job_Type;
                  Result : out Job_Result_Type);

end AdaSpec.Job;
