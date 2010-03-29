--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Strings;

package body AdaSpecLib is

   function To_String (Pos : in Position_Type) return String is
      use Ada.Strings.Fixed;
      use Ada.Strings.Unbounded;
      use Ada.Strings;
   begin
      return To_String (Pos.File) & ":" & Trim (Pos.Line'Img, Left);
   end To_String;

   function Position  (File : in String;
                       Line : in Positive)      return Position_Type
   is
      use Ada.Strings.Unbounded;
   begin
      return Position_Type'
         (File => To_Unbounded_String (File),
          Line => Line);
   end Position;

   function Is_Null   (Pos  : in Position_Type) return Boolean is
   begin
      return Pos.Line = 0;
   end Is_Null;

end AdaSpecLib;
