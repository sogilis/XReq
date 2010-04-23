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

      2 scenarios (1 failed, 1 passed)
      5 steps (1 failed, 1 skipped, 3 passed)

      """
