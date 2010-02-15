--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

use Ada.Strings.Unbounded;

package AdaSpec.Steps is

   type Step_File_Type is abstract tagged private;
   type Step_File_Ptr  is access all Step_File_Type'Class;

   package Step_Vectors is
      new Ada.Containers.Vectors (Natural, Step_File_Ptr, "=");

   Unparsed_Step : exception;

   function  File_Name (S : in Step_File_Type) return String;
   function  Parsed    (S : in Step_File_Type) return Boolean is abstract;
   procedure Parse     (S : in out Step_File_Type) is abstract;

   function  Contains  (S      : in Step_File_Type;
                        Prefix : in Prefix_Type;
                        Phrase : in String) return Boolean is abstract;

private

   type Step_File_Type is abstract tagged
      record
         File_Name : Unbounded_String;
      end record;

end AdaSpec.Steps;
