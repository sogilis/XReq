--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Util.Strings;

use Ada.Strings.Unbounded;

package AdaSpec.Stanzas is

   -------------------
   --  Stanza_Type  --
   -------------------

   type Stanza_Type is
      record
         Prefix : Prefix_Type;
         Stanza : Unbounded_String;
         Texts  : Util.Strings.Vectors.Vector;
         Pos    : Position_Type;
      end record;
   type Stanza_Ptr is access all Stanza_Type;
   Null_Stanza : Stanza_Type;

   package Stanza_Container is
      new Ada.Containers.Vectors (Natural, Stanza_Type, "=");

   function Stanza_Given (S    : in String;
                          File : in String := "";
                          Line : in Positive := 1) return Stanza_Type;
   function Stanza_When  (S : in String;
                          File : in String := "";
                          Line : in Positive := 1) return Stanza_Type;
   function Stanza_Then  (S : in String;
                          File : in String := "";
                          Line : in Positive := 1) return Stanza_Type;

   function To_String (S : in Stanza_Type) return String;
   function To_Regexp (S : in Stanza_Type) return String;
   function Position  (S : in Stanza_Type) return String;

--    function "=" (Left, Right : in Stanza_Type) return Boolean;

end AdaSpec.Stanzas;
