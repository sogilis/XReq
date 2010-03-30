--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;

package AdaSpecLib is

   Not_Yet_Implemented : exception;

   -----------------
   --  Step_Type  --
   -----------------

   type Step_All_Kind is (Step_Null, Step_Given, Step_When, Step_Then);

   subtype Step_Kind is Step_All_Kind range Step_Given .. Step_Then;

   ---------------------
   --  Position_Type  --
   ---------------------

   type Position_Type is
      record
         File : Ada.Strings.Unbounded.Unbounded_String;
         Line : Natural := 0;
      end record;

   function To_String (Pos : in Position_Type) return String;


end AdaSpecLib;
