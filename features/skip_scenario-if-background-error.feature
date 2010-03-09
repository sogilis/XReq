Feature: Skip scenarios after a background error
  In order to avoid executing steps that are sure to fail
  As an adaspec user
  I want to avoid executing steps when the background was not successfull.

  If the first background is unsuccessfull, then the feature should stop
  alltogether. But if a background fail just before a scenario that is not the
  first, just skip this scenario.

  Background: Set things up
    Given adaspec is in the PATH
    And I am in an empty directory
    And a file "features/always_fail.feature":
      """
      Feature: Always Fail

        Background: Set things up
          Given this step works BACKGROUND
          And it fail
          And this step works BACKGROUND

        Scenario: Run a good step 1
          Given this step works STEP
          And this is ignored

        Scenario: Run a good step 2
          Given this step works STEP
          And this is ignored

        Scenario: Run a good step 3
          Given this step works STEP
          And this is ignored

        Scenario: Run a good step 4
          Given this step works STEP
          And this is ignored

      """
    And a file "features/periodic_fail.feature":
      """
      Feature: Periodic Fail

        Background: Set things up
          Given this step works BACKGROUND
          And this fails periodically
          And this step works BACKGROUND

        Scenario: Run a good step 1
          Given this step works STEP
          And this is ignored

        Scenario: Run a good step 2
          Given this step works STEP
          And this is ignored

        Scenario: Run a good step 3
          Given this step works STEP
          And this is ignored

        Scenario: Run a good step 4
          Given this step works STEP
          And this is ignored

      """
    And a file "features/step_definitions/steps.ads":
      """
      with AdaSpecLib;
      use  AdaSpecLib;
      package Steps is

        --  @given ^this step works(.*)$
        procedure This_Step_Works (Args : in out Arg_Type);

        --  @given ^this fails periodically$
        procedure Periodic_Fail (Args : in out Arg_Type);

        --  @given ^it fail$
        procedure Make_It_Fail (Args : in out Arg_Type);

        --  @then ^do nothing$
        --  @given ^this is ignored$
        procedure Do_Nothing (Args : in out Arg_Type) is null;

      end Steps;
      """
    And a file "features/step_definitions/steps.adb":
      """
      with Ada.Text_IO;
      use Ada.Text_IO;
      package body Steps is

        Num : Positive := 2;

        procedure This_Step_Works (Args : in out Arg_Type) is
          pragma Unreferenced (Args);
        begin
          Put_Line ("This step works" & Args.Match (1));
        end This_Step_Works;

        procedure Periodic_Fail (Args : in out Arg_Type) is
        begin
          Num := Num + 1;
          if Num = 3 then
            Num := 1;
          end if;
          Assert (Num = 1, "Num =" & Num'Img & " /= 1");
          Put_Line ("Num =" & Num'Img & " = 1 OK");
        end Periodic_Fail;

        procedure Make_It_Fail (Args : in out Arg_Type) is
          pragma Unreferenced (Args);
        begin
          Assert (False, "Error message");
        end Make_It_Fail;

      end Steps;
      """

  Scenario: Always Fail
    When I run adaspec -x always_fail_suite features/always_fail.feature
    Then it should pass
    When I compile "always_fail_suite" in features/tests
    Then it should pass
    When I run "./always_fail_suite" in features/tests
    Then it should fail with
      """
      Feature: Always Fail

        Background: Set things up
      This step works BACKGROUND
          Given this step works BACKGROUND
          And it fail
            ADASPECLIB.ERROR: Error message
          And this step works BACKGROUND

        Scenario: Run a good step 1
          Given this step works STEP
          And this is ignored

        Scenario: Run a good step 2
          Given this step works STEP
          And this is ignored

        Scenario: Run a good step 3
          Given this step works STEP
          And this is ignored

        Scenario: Run a good step 4
          Given this step works STEP
          And this is ignored

      4 scenarios (4 failed)
      20 steps (1 failed, 18 skipped, 1 passed)

      """


  Scenario: Periodic Fail
    When I run adaspec -x periodic_fail_suite features/periodic_fail.feature
    Then it should pass
    When I compile "periodic_fail_suite" in features/tests
    Then it should pass
    When I run "./periodic_fail_suite" in features/tests
    Then it should fail with
      """
      Feature: Periodic Fail

        Background: Set things up
      This step works BACKGROUND
          Given this step works BACKGROUND
      Num = 1 = 1 OK
          And this fails periodically
      This step works BACKGROUND
          And this step works BACKGROUND

        Scenario: Run a good step 1
      This step works STEP
          Given this step works STEP
          And this is ignored

        Scenario: Run a good step 2
      This step works BACKGROUND
          Given this fails periodically
            ADASPECLIB.ERROR: Num = 2 /= 1
          And this step works BACKGROUND
          And this step works STEP
          And this is ignored

        Scenario: Run a good step 3
      This step works BACKGROUND
      Num = 1 = 1 OK
      This step works BACKGROUND
      This step works STEP
          Given this step works STEP
          And this is ignored

        Scenario: Run a good step 4
      This step works BACKGROUND
          Given this fails periodically
            ADASPECLIB.ERROR: Num = 2 /= 1
          And this step works BACKGROUND
          And this step works STEP
          And this is ignored

      4 scenarios (2 failed, 2 passed)
      20 steps (2 failed, 6 skipped, 12 passed)

      """