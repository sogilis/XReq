Feature: debug mode for test suites
  In order to debug errors in features
  As an XReq user
  I want to be able to see additional information on each step when the debug
  mode is active

  Background:
    Given xreq is in the PATH
    And I am in the xreq directory
    And a file "features/data/tmp-debug-mode.feature":
      """
      Feature: Debug

        Scenario: A
          Given this step works
          And this step doesn't work
          And this step works with BAR

        Scenario: B
          Given this step works with FOO
          And this step works with BAR
      """

  @lang @lang-Ada
  Scenario: Text mode
    When I run xreq -m -x suite features/data/tmp-debug-mode.feature
    Then it should pass
    And  "features/data/tests/suite" should exist
    When I run the test suite "features/data/tests/suite -d"
    Then it should fail with
      """
      Feature: Debug

        Scenario: A
          Given this step works
          And this step doesn't work
            XREQLIB.ASSERTS.ERROR: Assertion failed
          And this step works with BAR

        Scenario: B
      This step works FOO
          Given this step works with FOO
            -------------------------------------------------------------------------
            Debug text for working step FOO
      This step works BAR
          And this step works with BAR
            -------------------------------------------------------------------------
            Debug text for working step BAR

      features/data/tests/suite features/data/tmp-debug-mode.feature:3

      2 scenarios (1 failed, 1 passed)
      5 steps (1 failed, 1 skipped, 3 passed)

      """

  @lang @lang-C
  Scenario: Text mode
    When I run xreq -m -x suite features/data/tmp-debug-mode.feature
    Then it should pass
    And  "features/data/tests/suite" should exist
    When I run the test suite "features/data/tests/suite -d"
    Then it should fail with
      """
      Feature: Debug

        Scenario: A
          Given this step works
          And this step doesn't work
            XREQLIB.C_INTERFACE.XREQ_FORMAT_PUT_ERROR.STEP_ERROR: Assertion failed
      in: ../step_definitions/simple_steps.c:28
          And this step works with BAR

        Scenario: B
      This step works FOO
          Given this step works with FOO
            -------------------------------------------------------------------------
            Debug text for working step FOO
      This step works BAR
          And this step works with BAR
            -------------------------------------------------------------------------
            Debug text for working step BAR

      2 scenarios (1 failed, 1 passed)
      5 steps (1 failed, 1 skipped, 3 passed)

      """

  Scenario: HTML
    When I run xreq -m -x suite features/data/tmp-debug-mode.feature
    Then it should pass
    And  "features/data/tests/suite" should exist
    When I run the test suite "features/data/tests/suite -d -f html -o reports/sample-html-debug.html"
    Then it should fail
    And  "reports/sample-html-debug.html" should contain
      """
                <hr />
                <p>Debug text for working step FOO</p>
      """
    And  "reports/sample-html-debug.html" should contain
      """
                <hr />
                <p>Debug text for working step BAR</p>
      """
