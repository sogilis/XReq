with XReqLib.Asserts;
use  XReqLib.Asserts;

package body Tables is

   T : Table_Type;

   procedure Given_a_table (Args : in out Arg_Type) is
   begin
      T := Args.Table;
   end Given_a_table;

   procedure Then_the_table_should_be_equal_to (Args : in out Arg_Type) is
   begin
      Assert (T = Args.Table);
   end Then_the_table_should_be_equal_to;

end Tables;
