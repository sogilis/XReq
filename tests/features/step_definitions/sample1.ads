--                         Copyright (C) 2010, Sogilis                       --

with AdaSpecLib.General;

use  AdaSpecLib.General;

package Sample1 is

   --  @given ^this step works$
   procedure This_Step_Works (Args : in out Arg_Type);

   --  @when ^this step works too$
   procedure This_Step_Works_Too (Args : in out Arg_Type);

   --  @given

   --  @when toto

   --  @then tata

end Sample1;
