--                         Copyright (C) 2010, Sogilis                       --

with AdaSpecLib.General;

use  AdaSpecLib.General;

package Sample2 is

   --  @given ^I am in front of a cake machine$
   procedure I_am_in_front_of_a_cake_machine (Args : in out Arg_Type);

   --  @when ^I insert money$
   procedure I_insert_money (Args : in out Arg_Type);

   --  @when ^I push the button$
   procedure I_push_the_button (Args : in out Arg_Type);

   --  @then ^I get a cake$
   procedure I_get_a_cake (Args : in out Arg_Type);

   --  @when ^I match "(.*)"$
   procedure When_I_Match (Args : in out Arg_Type);

end Sample2;
