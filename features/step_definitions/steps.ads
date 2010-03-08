--                         Copyright (C) 2010, Sogilis                       --

with AdaSpecLib;
use  AdaSpecLib;

package Steps is

   --  @given ^adaspec is in the PATH$
   procedure AdaSpec_in_path (Args : in out Arg_Type);

   --  @given ^I am in an empty directory$
   procedure I_am_empty_dir (Args : in out Arg_Type);

   --  @given ^a file "(.*)":$
   procedure Given_a_file (Args : in out Arg_Type);

   --  @when  ^I run adaspec (.*)$
   procedure I_run_adaspec (Args : in out Arg_Type);

   --  @then  ^it should (pass|fail)$
   procedure it_should_pass_fail (Args : in out Arg_Type);

   --  @then  ^it should (pass|fail) with$
   procedure it_should_pass_fail_with (Args : in out Arg_Type);

   --  @given ^the sources of adaspec are in ADA_INCLUDE_PATH$
   --  @given ^I am in the adaspec directory$
   --  @given ^I am in "(.*)"$
   --  @when  ^I compile "(.*)" in (.*)$
   --  @when  ^I run "(.*)" in (.*)$
   --  @when  ^I run "(.*)"$
   --  @when  ^I run '(.*)' aloud$
   --  @then  ^the output should contain$
   --  @then  ^"([^\"]*)" should contain$
   --  @then  ^"([^\"]*)" should exist$
   --  @then  ^"([^\"]*)" (should not|shouldn't) exist$

   procedure Not_Yet_Implemented (Args : in out Arg_Type);

end Steps;
