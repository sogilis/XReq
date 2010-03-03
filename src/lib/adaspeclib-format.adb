--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with AdaSpecLib.Format.Text;

package body AdaSpecLib.Format is

   function Get_Formatter (Name : in String) return Format_Ptr is
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps.Constants;
      N : constant String := Translate (Name, Lower_Case_Map);
   begin
      if N = "text" then
         return Format_Ptr (AdaSpecLib.Format.Text.New_Text_Format);
      else
         return null;
      end if;
   end Get_Formatter;  --  GCOV_IGNORE

end AdaSpecLib.Format;