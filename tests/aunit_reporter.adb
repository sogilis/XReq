--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;

use Ada.Text_IO;

package body AUnit_Reporter is

   overriding procedure Report (Engine : in Reporter;
                                R  : in out AUnit.Test_Results.Result'Class)
   is
      Old_Output : constant File_Type := Current_Output;
      Old_Error  : constant File_Type := Current_Error;
   begin

      Set_Output (Engine.File.all);
      Set_Error  (Engine.File.all);

      GNAT.IO.Set_Output (Engine.GNAT_IO.all);

      Engine.Reporter.all.Report (R);

      Set_Output (Old_Output);
      Set_Error  (Old_Error);

   end Report;

end AUnit_Reporter;
