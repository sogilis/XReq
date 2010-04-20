with XReqLib.General;
use  XReqLib.General;

package Strings is

   --  @when ^I concatenate "(.*)" and "(.*)"$
   procedure When_I_concatenate_and (Args : in out Arg_Type);

   --  @then ^I get "(.*)"$
   procedure Then_I_Get (Args : in out Arg_Type);

   --  @given ^the long string:$
   procedure Given_the_long_string (Args : in out Arg_Type);

   --  @when ^I compare it with "(.*)"$
   procedure When_I_compare_it_with (Args : in out Arg_Type);

   --  @then ^they are equal$
   procedure Then_they_are_equal (Args : in out Arg_Type);

end Strings;
