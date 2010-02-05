--                         Copyright (C) 2010, Sogilis                       --

with Ada.Containers.Vectors;
with GNAT.Regexp;

use Ada.Containers;
use GNAT.Regexp;

package AdaSpec.Steps.Ada is

   type Ada_Step_File_Type is new Step_File_Type with private;
   type Ada_Step_File_Ptr  is access all Ada_Step_File_Type'Class;

   procedure Make (S         : in out Ada_Step_File_Type;
                   File_Name : in String);

   overriding function  Parsed    (S : in Ada_Step_File_Type) return Boolean;
   overriding procedure Parse     (S : in out Ada_Step_File_Type);

   overriding function  Contains  (S      : in Ada_Step_File_Type;
                                   Prefix : in Prefix_Type;
                                   Phrase : in String) return Boolean;

private

   type Step_Type is
      record
         Prefix  : Prefix_Type;
         Pattern : Regexp;
         Pat_S   : Unbounded_String;
      end record;

   package Step_Container is new Vectors (Natural, Step_Type);
   use Step_Container;

   type Ada_Step_File_Type is new Step_File_Type with
      record
         Parsed : Boolean := False;
         Steps  : Step_Container.Vector;
      end record;

end AdaSpec.Steps.Ada;
