--                         Copyright (C) 2010, Sogilis                       --

package body AdaSpecLib.Asserts is

   --------------
   --  Assert  --
   --------------

   procedure Assert (Cmp : in Boolean; Reason : in String := "") is
   begin
      if not Cmp then
         if Reason /= "" then
            raise Error with Reason;
         else
            raise Error with "Assertion failed";
         end if;
      end if;
   end Assert;

   --------------
   --  Equals  --
   --------------

   procedure Equals (T1, T2 : in String; Reason : in String := "") is
   begin
      if T1 /= T2 then
         if Reason /= "" then
            raise Error with Reason & ASCII.LF &
               "Expected two strings to be equals:" & ASCII.LF &
               T1 & ASCII.LF & "--  is not the same as  --" & ASCII.LF &
               T2 & ASCII.LF & "--";
         else
            raise Error with
               "Expected two strings to be equals:" & ASCII.LF &
               T1 & ASCII.LF & "--  is not the same as  --" & ASCII.LF &
               T2 & ASCII.LF & "--";
         end if;
      end if;
   end Equals;

end AdaSpecLib.Asserts;
