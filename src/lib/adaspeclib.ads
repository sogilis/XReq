--                         Copyright (C) 2010, Sogilis                       --

package AdaSpecLib is

   Not_Yet_Implemented : exception;

   -----------------
   --  Step_Type  --
   -----------------

   type Step_All_Kind is (Step_Null, Step_Given, Step_When, Step_Then);

   subtype Step_Kind is Step_All_Kind range Step_Given .. Step_Then;


end AdaSpecLib;
