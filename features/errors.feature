Feature: Error handling in steps
  In order to habdle scenarios that fail
  As an AdaSpec user
  I want to be able to see where a step failed
  And to see if other scenarios passed or not

  Background:
    Given adaspec is in the PATH
    And I am in an empty directory
    And a file "features/simple_error.feature":
      """
      Feature: Sample

        Background: B
          Given this step works

        Scenario: Run a bad step
          Given this step doesn't work
          Given this step works

        Scenario: Run a good step
          Given this step works

      """
    And a file "features/step_definitions/steps.ads":
      """
      with AdaSpecLib.General;
      use  AdaSpecLib.General;
      package Steps is

        --  @given ^this step works$
        procedure This_Step_Works (Args : in out Arg_Type);

        --  @given ^this step doesn't work$
        procedure This_Step_Doest_Work (Args : in out Arg_Type);

      end Steps;
      """
    And a file "features/step_definitions/steps.adb":
      """
      with AdaSpecLib.Asserts;
      use  AdaSpecLib.Asserts;
      package body Steps is

        procedure This_Step_Works (Args : in out Arg_Type) is
          pragma Unreferenced (Args);
        begin
          null;
        end This_Step_Works;

        procedure This_Step_Doest_Work (Args : in out Arg_Type) is
          pragma Unreferenced (Args);
        begin
          null;
          Assert (False);
        end This_Step_Doest_Work;

      end Steps;
      """

  Scenario: Test error reporting
    When I run adaspec -x suite features/simple_error.feature
    Then it should pass
    When I compile "suite" in features/tests
    Then it should pass
    When I run "./suite" in features/tests
    Then it should fail with
      """
      Feature: Sample

        Background: B
          Given this step works

        Scenario: Run a bad step
          Given this step doesn't work
            ADASPECLIB.ASSERTS.ERROR: Assertion failed
          And this step works

        Scenario: Run a good step
          Given this step works

      2 scenarios (1 failed, 1 passed)
      5 steps (1 failed, 1 skipped, 3 passed)

      """