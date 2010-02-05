--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;

package AdaSpec.Features is

   type Feature_File_Type is tagged private;
   type Feature_File_Ptr  is access all Feature_File_Type'Class;

   Unparsed_Feature : exception;

   procedure Make (S         : in out Feature_File_Type;
                   File_Name : in String);

   function  File_Name (S : in Feature_File_Type) return String;
   function  Parsed    (S : in Feature_File_Type) return Boolean;
   procedure Parse     (S : in out Feature_File_Type);

private

   type Feature_File_Type is tagged
      record
         File_Name : Unbounded_String;
         Parsed    : Boolean := False;
      end record;

end AdaSpec.Features;
