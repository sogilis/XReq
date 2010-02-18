--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Sets;

use Ada.Strings.Unbounded;

package Util.Strings is

   package Vectors is
      new Ada.Containers.Vectors (Natural, Unbounded_String, "=");

   type String_List is  --  GCOV_IGNORE
      array (Positive range <>) of Unbounded_String;

   package String_Set is
      new Ada.Containers.Hashed_Sets (Unbounded_String, Hash, "=", "=");


   procedure Find_Token (Search     : in String;
                         Tokens     : in String_List;
                         Index_Next : out Natural;
                         Token      : out Natural);

   function Starts_With (Search      : in String;
                         Pattern     : in String;
                         Start_Index : in Natural := 1) return Boolean;

   function Trimed_Suffix (Source      : in Unbounded_String;
                           Start_Index : in Natural) return Unbounded_String;

   function Trimed_Suffix (Source      : in String;
                           Start_Index : in Natural) return String;

   function To_Identifier (Source : in String) return String;  --  GCOV_IGNORE

end Util.Strings;
