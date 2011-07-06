with Calculator_Driver;
with XReqLib.Asserts;

package body Generated_Steps is

   use Calculator_Driver;
   use XReqLib.Asserts;

   procedure Given_the_calculator_is_initialized (Args : in out Arg_Type) is
   begin
      Calculator_Driver.Calculator_Driver.Initialize;
   end Given_the_calculator_is_initialized;

   procedure Given_the_first_operand (Args : in out Arg_Type) is
   begin
      Calculator_Driver.Calculator_Driver.First_Operand (Integer'Value (Args.Match (1)));
   end Given_the_first_operand;

   procedure Given_the_second_operand (Args : in out Arg_Type) is
   begin
      Calculator_Driver.Calculator_Driver.Second_Operand (Integer'Value (Args.Match (1)));
   end Given_the_second_operand;

   procedure When_the_two_operands_are_added (Args : in out Arg_Type) is
   begin
      Calculator_Driver.Calculator_Driver.Add;
   end When_the_two_operands_are_added;

   procedure Then_the_result_should_be (Args : in out Arg_Type) is
      res : integer;
   begin
      Calculator_Driver.Calculator_Driver.Get_Result (res);
      Args.Add_Para ("Got:" & res'Img);
      Assert (res = Integer'Value (Args.Match (1)));
   end Then_the_result_should_be;

end Generated_Steps;
