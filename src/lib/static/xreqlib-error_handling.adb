--                         Copyright (C) 2010, Sogilis                       --

with GNAT.Traceback.Symbolic;

package body XReqLib.Error_Handling is

   function Symbolic_Traceback (E : Exception_Occurrence) return String is
   begin
      return GNAT.Traceback.Symbolic.Symbolic_Traceback (E);
   end Symbolic_Traceback;

end XReqLib.Error_Handling;
