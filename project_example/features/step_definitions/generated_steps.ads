with XReqLib.General;
use  XReqLib.General;
package Generated_Steps is

   --  @given ^the calculator is initialized$
   procedure Given_the_calculator_is_initialized (Args : in out Arg_Type);

   --  @given ^the first operand: ([0-9]+)$
   procedure Given_the_first_operand (Args : in out Arg_Type);

   --  @given ^the second operand: ([0-9]+)$
   procedure Given_the_second_operand (Args : in out Arg_Type);

   --  @when ^the two operands are added$
   procedure When_the_two_operands_are_added (Args : in out Arg_Type);

   --  @then ^the result should be: ([0-9]+)$
   procedure Then_the_result_should_be (Args : in out Arg_Type);

end Generated_Steps;
