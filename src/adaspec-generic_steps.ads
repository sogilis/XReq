--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;

generic

   type Argument_Type is private;
   with function "=" (Left, Right : in Argument_Type) return Boolean;

package AdaSpec.Generic_Steps is

   -------------------
   --  Stanza_Type  --
   -------------------

   type Step_Type is tagged private;
   type Step_Ptr is access all Step_Type'Class;

   function  Stanza_Given (S    : in String;
                           File : in String := "";
                           Line : in Positive := 1) return Step_Type;
   function  Stanza_When  (S : in String;
                           File : in String := "";
                           Line : in Positive := 1) return Step_Type;
   function  Stanza_Then  (S : in String;
                           File : in String := "";
                           Line : in Positive := 1) return Step_Type;

   function  New_Step     (Kind     : in Prefix_Type;
                           Stanza   : in String;
                           Position : in Position_Type) return Step_Type;

   function  To_String    (S : in Step_Type) return String;
   function  To_Regexp    (S : in Step_Type) return String;

   function  Arg_First    (S : in Step_Type) return Natural;
   function  Arg_Last     (S : in Step_Type) return Natural;
   function  Arg_Element  (S : in Step_Type;
                           I : in Natural)   return Argument_Type;

   function  Position     (S : in Step_Type) return Position_Type;
   function  Stanza       (S : in Step_Type) return String;
   function  Kind         (S : in Step_Type) return Prefix_Type;

   procedure Set_Position (S      : in out Step_Type;
                           Pos    : in     Position_Type);
   procedure Set_Stanza   (S      : in out Step_Type;
                           Stanza : in     String);
   procedure Set_Kind     (S      : in out Step_Type;
                           Kind   : in     Prefix_Type);
   procedure Arg_Append   (S      : in out Step_Type;
                           E      : in     Argument_Type);

   Null_Step : constant Step_Type;

private

   package Argument_Vectors is new
      Ada.Containers.Vectors (Natural, Argument_Type, "=");

   type Step_Type is tagged
      record
         Prefix : Prefix_Type;
         Stanza : Unbounded_String;
         Args   : Argument_Vectors.Vector;
         Pos    : Position_Type;
      end record;

   Null_Step : constant Step_Type := (others => <>);

end AdaSpec.Generic_Steps;
