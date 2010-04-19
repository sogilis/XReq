--                         Copyright (C) 2010, Sogilis                       --

package XReqLib.Report is

   -------------------
   --  Report_Type  --
   -------------------

   --  GCOV_IGNORE_BEGIN
   type Report_Type is
      record
         Count_Scenario_Failed : Natural := 0;
         Count_Scenario_Passed : Natural := 0;
         Count_Steps_Failed    : Natural := 0;
         Count_Steps_Skipped   : Natural := 0;
         Count_Steps_Passed    : Natural := 0;
         Num_Steps             : Natural := 0;
      end record;
   --  GCOV_IGNORE_END

   function Status (Report : in Report_Type) return Boolean;

end XReqLib.Report;
