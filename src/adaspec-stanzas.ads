--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;

package AdaSpec.Stanzas is

   -------------------
   --  Stanza_Type  --
   -------------------

   type Stanza_Type is
      record
         Prefix : Prefix_Type;
         Stanza : Unbounded_String;
      end record;
   Null_Stanza : Stanza_Type;

   package Stanza_Container is
      new Ada.Containers.Vectors (Natural, Stanza_Type, "=");

   function Stanza_Given (S : in String) return Stanza_Type;
   function Stanza_When  (S : in String) return Stanza_Type;
   function Stanza_Then  (S : in String) return Stanza_Type;

end AdaSpec.Stanzas;