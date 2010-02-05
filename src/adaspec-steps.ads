--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;

package AdaSpec.Steps is

   type Prefix_Type is (Prefix_Given, Prefix_When, Prefix_Then);

   type Step_Type is abstract tagged private;
   type Step_Ptr  is access all Step_Type'Class;

   Unparsed_Step : exception;

   function  File_Name (S : in Step_Type) return String;
   function  Parsed    (S : in Step_Type) return Boolean is abstract;
   procedure Parse     (S : in out Step_Type) is abstract;

   function  Contains  (S      : in Step_Type;
                        Prefix : in Prefix_Type;
                        Phrase : in String) return Boolean is abstract;

private

   type Step_Type is abstract tagged
      record
         File_Name : Unbounded_String;
      end record;

end AdaSpec.Steps;
