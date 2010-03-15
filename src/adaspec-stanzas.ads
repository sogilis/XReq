--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with AdaSpecLib.String_Tables;

use Ada.Strings.Unbounded;

package AdaSpec.Stanzas is

   type Argument_Kind is (None, Text, Table);

   type Argument_Type (Typ : Argument_Kind := None) is
      record
         case Typ is
            when Text =>
               Text : Unbounded_String;
            when Table =>
               Table : AdaSpecLib.String_Tables.Table;
            when None =>
               null;
         end case;
      end record;

   package Argument_Vectors is new
      Ada.Containers.Vectors (Natural, Argument_Type, "=");

   -------------------
   --  Stanza_Type  --
   -------------------

   type Stanza_Type is
      record
         Prefix : Prefix_Type;
         Stanza : Unbounded_String;
         Args   : Argument_Vectors.Vector;
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
