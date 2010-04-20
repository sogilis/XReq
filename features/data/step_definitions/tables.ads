with XReqLib.General;
use  XReqLib.General;

package Tables is

   --  @given ^a table:$
   procedure Given_a_table (Args : in out Arg_Type);

   --  @then ^the table should be equal to:$
   procedure Then_the_table_should_be_equal_to (Args : in out Arg_Type);

end Tables;
