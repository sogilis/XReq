--                         Copyright (C) 2010, Sogilis                       --

with Ada.Exceptions;

use Ada.Exceptions;

package AdaSpecLib.Format.Text is

   procedure Put_Feature    (Feature    : in String);
   procedure Put_Background (Background : in String);
   procedure Put_Scenario   (Scenario   : in String);
   procedure Put_Step       (Step       : in Step_Type;
                             Name       : in String);
   procedure Put_Error      (Err        : in Exception_Occurrence);
   procedure Put_Summary    (Report     : in Report_Type);

end AdaSpecLib.Format.Text;
