--                         Copyright (C) 2010, Sogilis                       --

with Ada.Text_IO;

package body XReqLib.Util is

   procedure Put_Exception_Information (X : in Exception_Occurrence) is
      use Ada.Text_IO;
   begin
      Put_Line (Current_Error, Exception_Information (X));
   end Put_Exception_Information;

end XReqLib.Util;
