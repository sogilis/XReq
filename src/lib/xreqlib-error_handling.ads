--                         Copyright (C) 2010, Sogilis                       --

with Ada.Exceptions;

use Ada.Exceptions;

package XReqLib.Error_Handling is

   function Symbolic_Traceback (E : Exception_Occurrence) return String;

end XReqLib.Error_Handling;
