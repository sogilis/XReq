--                         Copyright (C) 2010, Sogilis                       --

package AdaSpec is

   Not_Yet_Implemented : exception;

   type Prefix_Type_Maybe is
      (Prefix_None, Prefix_Given, Prefix_When, Prefix_Then);

   subtype Prefix_Type is Prefix_Type_Maybe range Prefix_Given .. Prefix_Then;

end AdaSpec;