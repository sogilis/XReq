with XReqLib.Asserts;
use  XReqLib.Asserts;

package body Cucumbers is

   Cukes : Integer := 0;

   procedure Given_there_are_n_cucumbers (Args : in out Arg_Type) is
   begin
      Cukes := Integer'Value (Args.Match (1));
   end Given_there_are_n_cucumbers;

   procedure When_i_eat_n_cucumbers (Args : in out Arg_Type) is
   begin
      Cukes := Cukes - Integer'Value (Args.Match (1));
   end When_i_eat_n_cucumbers;

   procedure Then_i_should_have_n_cucumbers (Args : in out Arg_Type) is
   begin
      Assert (Integer'Value (Args.Match (1)) = Cukes);
   end Then_i_should_have_n_cucumbers;

end Cucumbers;
