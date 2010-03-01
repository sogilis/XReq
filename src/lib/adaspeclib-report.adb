--                         Copyright (C) 2010, Sogilis                       --

package body AdaSpecLib.Report is

   --------------
   --  Status  --
   --------------

   function Status (Report : in Report_Type) return Boolean is
   begin
      return Report.Count_Scenario_Failed = 0 and
             Report.Count_Steps_Failed    = 0 and
             Report.Count_Steps_Skipped   = 0;
   end Status;

end AdaSpecLib.Report;
