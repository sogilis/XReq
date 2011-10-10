Feature: Error handling in steps
  In order to habdle scenarios that fail
  As an XReq user
  I want to be able to see where a step failed
  And to see if other scenarios passed or not

  Background:
    Given xreq is in the PATH
    And I am in the xreq directory
    And a file "features/data/tmp-simple_error.feature":
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

  @lang @lang-Ada
  Scenario: Test error reporting
    When I run xreq -m -x suite features/data/tmp-simple_error.feature
    Then it should pass
    When I run the test suite "./suite" in features/data/tests
    Then it should fail with
      """
      Feature: Sample

        Background: B
          Given this step works

        Scenario: Run a bad step
          Given this step doesn't work
            XREQLIB.ASSERTS.ERROR: Assertion failed
          And this step works

        Scenario: Run a good step
          Given this step works

      ./suite features/data/tmp-simple_error.feature:6

      2 scenarios (1 failed, 1 passed)
      5 steps (1 failed, 1 skipped, 3 passed)

      """

  @lang @lang-C
  Scenario: Test error reporting
    When I run xreq -m -x suite features/data/tmp-simple_error.feature
    Then it should pass
    When I run the test suite "./suite" in features/data/tests
    Then it should fail with
      """
      Feature: Sample

        Background: B
          Given this step works

        Scenario: Run a bad step
          Given this step doesn't work
            XREQLIB.C_INTERFACE.XREQ_FORMAT_PUT_ERROR.STEP_ERROR: Assertion failed
      in: ../step_definitions/simple_steps.c:28
          And this step works

        Scenario: Run a good step
          Given this step works

      2 scenarios (1 failed, 1 passed)
      5 steps (1 failed, 1 skipped, 3 passed)

      """
