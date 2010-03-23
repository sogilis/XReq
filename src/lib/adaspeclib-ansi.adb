--                         Copyright (C) 2010, Sogilis                       --

package body AdaSpecLib.ANSI is


   function ANSI (Sequence : in String) return String is
   begin
      if Use_ANSI_Sequences then
         return ASCII.ESC & '[' & Sequence;
      else
         return "";
      end if;
   end ANSI;  --  GCOV_INGORE

end AdaSpecLib.ANSI;
