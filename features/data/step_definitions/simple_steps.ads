with XReqLib.General;
use  XReqLib.General;

package Simple_Steps is

   --  @given ^this( step)? is ambiguous$
   --  @given ^this( step)? is( too)? ambiguous$
   --  @given ^this( step)? is ambiguous too$

   --  @given ^this is ignored$
   --  @then  ^do nothing$
   --  @given ^I match "([^"]*)" and "([^"]*)"$

   --  @given ^this step works$
   procedure Given_this_step_works
     (Args : in out Arg_Type);

   --  @given ^this step works with (.*)$
   procedure Given_this_step_works_with (Args : in out Arg_Type);

   --  @given ^this step works loudly$
   procedure Given_this_step_works_loudly (Args : in out Arg_Type);

   --  @given ^this step doesn't work$
   --  @given ^it fails$
   --  @when  ^it fails$
   procedure Given_this_step_doesn_t_work (Args : in out Arg_Type);

   --  @given ^this fails periodically$
   procedure Given_this_fails_periodically (Args : in out Arg_Type);

end Simple_Steps;
