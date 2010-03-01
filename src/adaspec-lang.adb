--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;

use Ada.Strings.Fixed;
use Ada.Strings.Maps.Constants;

package body AdaSpec.Lang is

   function Get_Language (Lang : in String) return Language_Type
   is
      L : constant String := Translate (Lang, Lower_Case_Map);
   begin
      if L = "ada" then
         return Lang_Ada;
      else
         raise Invalid_Language with "Unknown language " & Lang;
      end if;
   end Get_Language;

end AdaSpec.Lang;