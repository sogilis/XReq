with Ada.Text_IO;
with XReqLib.Asserts;

use  Ada.Text_IO;
use  XReqLib.Asserts;

package body Simple_Steps is

   State : Boolean := False;

   procedure Given_this_step_works (Args : in out Arg_Type) is
      pragma Unreferenced (Args);
   begin
      null;
   end Given_this_step_works;

   procedure Given_this_step_works_with (Args : in out Arg_Type) is
   begin
      Args.Add_Para ("Debug text for working step " & Args.Match (1));
      Put_Line ("This step works " & Args.Match (1));
   end Given_this_step_works_with;

   procedure Given_this_step_works_loudly (Args : in out Arg_Type) is
   begin
      Args.Add_Para ("Debug text");
      Put_Line ("This step works");
   end Given_this_step_works_loudly;

   procedure Given_this_step_doesn_t_work (Args : in out Arg_Type) is
      pragma Unreferenced (Args);
   begin
      Assert (False);
   end Given_this_step_doesn_t_work;

   procedure Given_this_fails_periodically (Args : in out Arg_Type) is
      pragma Unreferenced (Args);
   begin
      State := not State;
      Assert (State, "State is " & State'Img & " (should be TRUE)");
      Put_Line ("State is " & State'Img & " OK");
   end Given_this_fails_periodically;

end Simple_Steps;
