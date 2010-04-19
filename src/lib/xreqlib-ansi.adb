--                         Copyright (C) 2010, Sogilis                       --

package body XReqLib.ANSI is


   function ANSI (Sequence : in String) return String is
   begin
      if Use_ANSI_Sequences then
         return ASCII.ESC & '[' & Sequence;
      else
         return "";
      end if;
   end ANSI;  --  GCOV_IGNORE

end XReqLib.ANSI;
