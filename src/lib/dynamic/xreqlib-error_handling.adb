--                         Copyright (C) 2010, Sogilis                       --
--  The code source in this fime is taken entirely from GNAT.Traceback.Symbolic


package body XReqLib.Error_Handling is


   function Symbolic_Traceback (E : Exception_Occurrence) return String is
   begin
      --  TODO: GNAT.Traceback.Symbolic creates an undefined reference to the
      --        the symbol: gnat__traceback__symbolic__symbolic_traceback__2
      --        when acompiling a dynamic library
      return Exception_Information (E);
   end Symbolic_Traceback;

end XReqLib.Error_Handling;
