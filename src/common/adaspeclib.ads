--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;

package AdaSpecLib is

   Not_Yet_Implemented : exception;

   -----------------
   --  Step_Kind  --
   -----------------

   type Step_Kind is
      (Step_Given, Step_When, Step_Then, Step_Null);

   -----------------
   --  Step_Type  --
   -----------------

   type Step_Type is                            --  GCOV_IGNORE
      (Step_Given, Step_When, Step_Then);       --  GCOV_IGNORE
   pragma Obsolescent (Step_Type);

   ---------------------
   --  Position_Type  --
   ---------------------

   type Position_Type is
      record
         File : Ada.Strings.Unbounded.Unbounded_String;
         Line : Natural  := 0;
         Col  : Positive := 1;
      end record;

   function To_String (Pos  : in Position_Type) return String;
   function Position  (File : in String;
                       Line : in Positive)      return Position_Type;
   function Is_Null   (Pos  : in Position_Type) return Boolean;

   Null_Position : constant Position_Type;

end AdaSpecLib;
