--                         Copyright (C) 2010, Sogilis                       --

with AdaSpecLib.General;
use  AdaSpecLib.General;

package Steps is

   --  @given ^adaspec is in the PATH$
   procedure AdaSpec_in_path (Args : in out Arg_Type);

   --  @given ^the sources of adaspec are in ADA_INCLUDE_PATH$
   procedure Given_the_sources_of_Adaspec_are_in_path (Args : in out Arg_Type);

   --  @given ^I am in the adaspec directory$
   procedure Given_I_am_in_adaspec_dir (Args : in out Arg_Type);

   --  @given ^I am in "(.*)"$
   procedure Given_I_am_in (Args : in out Arg_Type);

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

   --  @when  ^I compile "(.*)" in (.*)$
   procedure when_I_compile_in (Args : in out Arg_Type);

   --  @when  ^I run "(.*)" in (.*)$
   procedure when_I_run_in (Args : in out Arg_Type);

   --  @when  ^I run the test suite "(.*)" in (.*)$
   --  @when  ^I run the test suite "(.*)"$
   procedure when_I_run_the_test_suite_in (Args : in out Arg_Type);

   --  @when  ^I run "(.*)"$
   procedure when_I_run (Args : in out Arg_Type);

   --  @then  ^"([^\"]*)" should exist$
   procedure Then_file_should_exist (Args : in out Arg_Type);

   --  @then  ^"([^\"]*)" (should not|shouldn't) exist$
   procedure Then_file_should_not_exist (Args : in out Arg_Type);

   --  @then  ^the output should contain$
   procedure Then_the_output_should_contain (Args : in out Arg_Type);

   --  @then  ^"([^\"]*)" should contain$
   procedure Then_the_file_should_contain (Args : in out Arg_Type);

   --  @when  ^I run '(.*)' aloud$

   procedure Not_Yet_Implemented (Args : in out Arg_Type);

end Steps;
