--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;

package Util.Strings is

   type String_List is array (Positive range <>) of Unbounded_String;

   procedure Find_Token (Search     : in String;
                         Tokens     : in String_List;
                         Index_Next : out Natural;
                         Token      : out Natural);

end Util.Strings;
