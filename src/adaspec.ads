--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with AdaSpecLib;

use AdaSpecLib;

package AdaSpec is

   Not_Yet_Implemented : exception;

   subtype Prefix_Type_Maybe is AdaSpecLib.Step_All_Kind;
   subtype Prefix_Type       is AdaSpecLib.Step_Kind;

--    type Prefix_Type_Maybe is
--       (Prefix_None, Prefix_Given, Prefix_When, Prefix_Then);

--    subtype Prefix_Type is
--       Prefix_Type_Maybe range Prefix_Given .. Prefix_Then; --  GCOV_IGNORE

   type Position_Type is
      record
         File : Ada.Strings.Unbounded.Unbounded_String;
         Line : Natural := 0;
      end record;

   function To_String (Pos : in Position_Type) return String;

end AdaSpec;
