--                         Copyright (C) 2010, Sogilis                       --

with Ada.Exceptions;

use Ada.Exceptions;

package XReqLib.Util is

   function Exception_Information (X : in Exception_Occurrence) return String
      renames Ada.Exceptions.Exception_Information;

   procedure Put_Exception_Information (X : in Exception_Occurrence);

end XReqLib.Util;
