--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

use Ada.Strings.Unbounded;

package Util.Strings is

   package Vectors is
      new Ada.Containers.Vectors (Natural, Unbounded_String, "=");

   type String_List is  --  GCOV_IGNORE
      array (Positive range <>) of Unbounded_String;

   procedure Find_Token (Search     : in String;
                         Tokens     : in String_List;
                         Index_Next : out Natural;
                         Token      : out Natural);

   function Starts_With (Search      : in String;
                         Pattern     : in String;
                         Start_Index : in Natural := 1) return Boolean;

   function Trimed_Suffix (Source      : in Unbounded_String;
                           Start_Index : in Natural) return Unbounded_String;

   function Trimed_Suffix (Source      : in String;  --  GCOV_IGNORE
                           Start_Index : in Natural) return String;

end Util.Strings;
