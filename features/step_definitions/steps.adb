--                         Copyright (C) 2010, Sogilis                       --

with Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;

package body Steps is

   Last_Exit_Code : Integer := 0;
   Last_Command_Output : Unbounded_String;

   procedure AdaSpec_in_path (Args : in out Arg_Type) is
   begin
      Not_Yet_Implemented (Args);
   end AdaSpec_in_path;

   procedure I_am_empty_dir (Args : in out Arg_Type) is
   begin
      Not_Yet_Implemented (Args);
   end I_am_empty_dir;

   procedure Given_a_file (Args : in out Arg_Type) is
   begin
      Not_Yet_Implemented (Args);
   end Given_a_file;

   procedure I_run_adaspec (Args : in out Arg_Type) is
   begin
      Not_Yet_Implemented (Args);
   end I_run_adaspec;

   procedure it_should_pass_fail (Args : in out Arg_Type) is
   begin
      if Args.Match (1) = "pass" then
         Assert (Last_Exit_Code = 0, "failed with code" & Last_Exit_Code'Img);
      else
         Assert (Last_Exit_Code /= 0, "succeed");
      end if;
   end it_should_pass_fail;

   procedure it_should_pass_fail_with (Args : in out Arg_Type) is
   begin
      it_should_pass_fail(Args);
      Assert (Args.Text = Last_Command_Output, "wrong output");
   end it_should_pass_fail_with;

   procedure Not_Yet_Implemented (Args : in out Arg_Type) is
   begin
      Assert (False, "Not Yet Implemented");
   end Not_Yet_Implemented;

end Steps;
