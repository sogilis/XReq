--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package AdaSpecLib is

   Not_Yet_Implemented : exception;

   ----------------------
   --  String_Vectors  --
   ----------------------

   package String_Vectors is new Ada.Containers.Vectors
     (Natural,
      Ada.Strings.Unbounded.Unbounded_String,
      Ada.Strings.Unbounded."=");

   subtype String_Vector is String_Vectors.Vector;

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
   function Position  (File : in String;
                       Line : in Natural) return Position_Type;

   Null_Position : constant Position_Type;

private

   Null_Position : constant Position_Type := (others => <>);

end AdaSpecLib;
