--                         Copyright (C) 2010, Sogilis                       --

with AdaSpecLib.Generic_Steps;
with AdaSpec.Args;

use AdaSpec.Args;

package AdaSpec.Steps is

   package Steps_Pkg is new AdaSpecLib.Generic_Steps (Argument_Type, "=");

   subtype Step_Type is Steps_Pkg.Step_Type;


   function  Stanza_Given (S    : in String;
                           File : in String := "";
                           Line : in Natural := 0) return Step_Type;
   function  Stanza_When  (S : in String;
                           File : in String := "";
                           Line : in Natural := 0) return Step_Type;
   function  Stanza_Then  (S : in String;
                           File : in String := "";
                           Line : in Natural := 0) return Step_Type;

   function Equals (Left, Right : in Step_Type) return Boolean;

end AdaSpec.Steps;

