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

   --------------
   --  Buffer  --
   --------------

   type Buffer_Type is tagged
      record
         Buffer : Unbounded_String;
         Ind    : Unbounded_String;
         CRLF   : Unbounded_String := To_Unbounded_String ("" & ASCII.LF);
      end record;

   procedure Put_Line   (Buffer : in out Buffer_Type; S : in String);
   procedure Put_Line   (Buffer : in out Buffer_Type; S : in Unbounded_String);
   procedure Put        (Buffer : in out Buffer_Type; S : in String);
   procedure Put        (Buffer : in out Buffer_Type; S : in Unbounded_String);
   procedure Put_Indent (Buffer : in out Buffer_Type);
   procedure New_Line   (Buffer : in out Buffer_Type);
   procedure Indent     (Buffer : in out Buffer_Type; N : Natural := 3);
   procedure UnIndent   (Buffer : in out Buffer_Type; N : Natural := 3);
   procedure Clear      (Buffer : in out Buffer_Type);
   function  --  GCOV_IGNORE
             Value      (Buffer : in     Buffer_Type) return String;

end Util.Strings;
