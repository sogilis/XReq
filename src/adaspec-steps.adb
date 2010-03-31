--                         Copyright (C) 2010, Sogilis                       --

with AdaSpecLib;

use AdaSpecLib;

package body AdaSpec.Steps is

   -----------------------------------
   --  Step_Type  --  Stanza_Given  --
   -----------------------------------

   function Stanza_Given (S    : in String;
                          File : in String := "";
                          Line : in Natural := 0) return Step_Type is
   begin
      return Step : Step_Type do
         Step.Make (Step_Given, S, Position (File, Line));
      end return;
   end Stanza_Given;

   ----------------------------------
   --  Step_Type  --  Stanza_When  --
   ----------------------------------

   function Stanza_When  (S    : in String;
                          File : in String := "";
                          Line : in Natural := 0) return Step_Type is
   begin
      return Step : Step_Type do
         Step.Make (Step_When, S, Position (File, Line));
      end return;
   end Stanza_When;

   ----------------------------------
   --  Step_Type  --  Stanza_Then  --
   ----------------------------------

   function Stanza_Then  (S    : in String;
                          File : in String := "";
                          Line : in Natural := 0) return Step_Type is
   begin
      return Step : Step_Type do
         Step.Make (Step_Then, S, Position (File, Line));
      end return;
   end Stanza_Then;

   -----------------------------
   --  Step_Type  --  Equals  --
   -----------------------------

   function Equals (Left, Right : in Step_Type) return Boolean is
      use Steps_Pkg;
   begin
      return Left = Right;
   end Equals;


end AdaSpec.Steps;
