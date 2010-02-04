--                         Copyright (C) 2010, Sogilis                       --

with Ada.Directories;

use Ada.Directories;

package body AdaSpec.Job is

   function Describe (Job : in Job_Type) return String is
      CRLF : constant String := ASCII.CR & ASCII.LF;
   begin
      return "Feature:     " & To_String (Job.Feature)  & CRLF &
             "Steps in:    " & To_String (Job.Step_Dir) & CRLF &
             "Generate in: " & To_String (Job.Out_Dir)  & CRLF;
   end Describe;

   procedure Fill_Missing (Job : in out Job_Type) is
   begin

      if Length (Job.Feature) = 0 then
         raise Invalid_Job;
      end if;

      if Length (Job.Step_Dir) = 0 then
         Job.Step_Dir := To_Unbounded_String (Compose (
            Containing_Directory (To_String (Job.Feature)), "steps"));
      end if;

      if Length (Job.Out_Dir) = 0 then
         Job.Out_Dir  := To_Unbounded_String (Compose (
            Containing_Directory (To_String (Job.Feature)), "tests"));
      end if;

   end Fill_Missing;

end AdaSpec.Job;
