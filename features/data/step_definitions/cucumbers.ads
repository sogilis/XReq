with XReqLib.General;
use  XReqLib.General;

package Cucumbers is

   --  @given ^there are ([0-9]+) cucumbers$
   procedure Given_there_are_n_cucumbers (Args : in out Arg_Type);

   --  @when ^I eat ([0-9]+) cucumbers$
   procedure When_i_eat_n_cucumbers (Args : in out Arg_Type);

   --  @then ^I should have ([0-9]+) cucumbers$
   procedure Then_i_should_have_n_cucumbers (Args : in out Arg_Type);

end Cucumbers;