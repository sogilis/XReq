with XReqLib.General;
use  XReqLib.General;

package Simple_Steps is

   --  @given ^this step is ambiguous$
   --  @given ^this step is( too)? ambiguous$

   --  @given ^this is ignored$
   --  @then  ^do nothing$

   --  @given ^this step works$
   procedure Given_this_step_works (Args : in out Arg_Type);

   --  @given ^this step works loudly$
   procedure Given_this_step_works_loudly (Args : in out Arg_Type);

   --  @given ^this step doesn't work$
   --  @when  ^it fails$
   procedure Given_this_step_doesn_t_work (Args : in out Arg_Type);

   --  @given ^this fails periodically$
   procedure Given_this_fails_periodically (Args : in out Arg_Type);

end Simple_Steps;
