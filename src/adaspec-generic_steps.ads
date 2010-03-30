--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;

generic
   type Argument_Type is private;
   with function "=" (Left, Right : in Argument_Type) return Boolean;
package AdaSpec.Generic_Steps is

   package Argument_Vectors is new
      Ada.Containers.Vectors (Natural, Argument_Type, "=");

   -------------------
   --  Stanza_Type  --
   -------------------

   type Step_Type is
      record
         Prefix : Prefix_Type;
         Stanza : Unbounded_String;
         Args   : Argument_Vectors.Vector;
         Pos    : Position_Type;
      end record;
   type Stanza_Ptr is access all Step_Type;
   Null_Stanza : Step_Type;

   package Stanza_Container is
      new Ada.Containers.Vectors (Natural, Step_Type, "=");

   function Stanza_Given (S    : in String;
                          File : in String := "";
                          Line : in Positive := 1) return Step_Type;
   function Stanza_When  (S : in String;
                          File : in String := "";
                          Line : in Positive := 1) return Step_Type;
   function Stanza_Then  (S : in String;
                          File : in String := "";
                          Line : in Positive := 1) return Step_Type;

   function To_String (S : in Step_Type) return String;
   function To_Regexp (S : in Step_Type) return String;
   function Position  (S : in Step_Type) return String;

end AdaSpec.Generic_Steps;
