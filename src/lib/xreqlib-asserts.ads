--                         Copyright (C) 2010, Sogilis                       --

package XReqLib.Asserts is

   --------------
   --  Assert  --
   --------------

   Error : exception;

   procedure Assert (Cmp : in Boolean; Reason : in String := "");
   procedure Equals (T1, T2 : in String; Reason : in String := "");

end XReqLib.Asserts;
