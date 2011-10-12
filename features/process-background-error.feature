Feature: Skip scenarios after a background error
  In order to avoid executing steps that are sure to fail
  As an xreq user
  I want to avoid executing steps when the background was not successfull.

  If the first background is unsuccessfull, then the feature should stop
  alltogether. But if a background fail just before a scenario that is not the
  first, just skip this scenario.

  Background: Set things up
    Given xreq is in the PATH
    And I am in the xreq directory
    And a file "features/data/tmp-always_fail.feature":
      """
      Feature: Always Fail

        Background: Set things up
          Given this step works with BACKGROUND
          And it fails
          And this step works with BACKGROUND

        Scenario: Run a good step 1
          Given this step works with STEP
          And this is ignored

        Scenario: Run a good step 2
          Given this step works with STEP
          And this is ignored

        Scenario: Run a good step 3
          Given this step works with STEP
          And this is ignored

        Scenario: Run a good step 4
          Given this step works with STEP
          And this is ignored

      """
    And a file "features/data/tmp-periodic_fail.feature":
      """
      Feature: Periodic Fail

        Background: Set things up
          Given this step works with BACKGROUND
          And this fails periodically
          And this step works with BACKGROUND

        Scenario: Run a good step 1
          Given this step works with STEP
          And this is ignored

        Scenario: Run a good step 2
          Given this step works with STEP
          And this is ignored

        Scenario: Run a good step 3
          Given this step works with STEP
          And this is ignored

        Scenario: Run a good step 4
          Given this step works with STEP
          And this is ignored

      """

  @lang @lang-Ada
  Scenario: Always Fail
    When I run xreq -m -x always_fail_suite features/data/tmp-always_fail.feature
    Then it should pass
    When I run the test suite "./always_fail_suite" in features/data/tests
    Then it should fail with
      """
      Feature: Always Fail

        Background: Set things up
      This step works BACKGROUND
          Given this step works with BACKGROUND
          And it fails
            XREQLIB.ASSERTS.ERROR: Assertion failed
          And this step works with BACKGROUND

        Scenario: Run a good step 1
          Given this step works with STEP
          And this is ignored

        Scenario: Run a good step 2
          Given this step works with STEP
          And this is ignored

        Scenario: Run a good step 3
          Given this step works with STEP
          And this is ignored

        Scenario: Run a good step 4
          Given this step works with STEP
          And this is ignored

      ./always_fail_suite features/data/tmp-always_fail.feature:8
      ./always_fail_suite features/data/tmp-always_fail.feature:12
      ./always_fail_suite features/data/tmp-always_fail.feature:16
      ./always_fail_suite features/data/tmp-always_fail.feature:20

      4 scenarios (4 failed)
      20 steps (1 failed, 18 skipped, 1 passed)

      """

  @lang @lang-C
  Scenario: Always Fail
    When I run xreq -m -x always_fail_suite features/data/tmp-always_fail.feature
    Then it should pass
    When I run the test suite "./always_fail_suite" in features/data/tests
    Then it should fail with
      """
      Feature: Always Fail

        Background: Set things up
      This step works BACKGROUND
          Given this step works with BACKGROUND
          And it fails
            XREQLIB.C_INTERFACE.XREQ_FORMAT_PUT_ERROR.STEP_ERROR: Assertion failed
      in: ../step_definitions/simple_steps.c:28
          And this step works with BACKGROUND

        Scenario: Run a good step 1
          Given this step works with STEP
          And this is ignored

        Scenario: Run a good step 2
          Given this step works with STEP
          And this is ignored

        Scenario: Run a good step 3
          Given this step works with STEP
          And this is ignored

        Scenario: Run a good step 4
          Given this step works with STEP
          And this is ignored

      ./always_fail_suite features/data/tmp-always_fail.feature:8

      4 scenarios (4 failed)
      20 steps (1 failed, 18 skipped, 1 passed)

      """

  @lang @lang-Ada
  Scenario: Periodic Fail
    When I run xreq -m -x periodic_fail_suite features/data/tmp-periodic_fail.feature
    Then it should pass
    When I run the test suite "./periodic_fail_suite" in features/data/tests
    Then it should fail with
      """
      Feature: Periodic Fail

        Background: Set things up
      This step works BACKGROUND
          Given this step works with BACKGROUND
      State is TRUE OK
          And this fails periodically
      This step works BACKGROUND
          And this step works with BACKGROUND

        Scenario: Run a good step 1
      This step works STEP
          Given this step works with STEP
          And this is ignored

        Scenario: Run a good step 2
      This step works BACKGROUND
          Given this fails periodically (background)
            XREQLIB.ASSERTS.ERROR: State is FALSE (should be TRUE)
          And this step works with BACKGROUND (background)
          And this step works with STEP
          And this is ignored

        Scenario: Run a good step 3
          Given this step works with STEP
          And this is ignored

        Scenario: Run a good step 4
          Given this step works with STEP
          And this is ignored

      ./periodic_fail_suite features/data/tmp-periodic_fail.feature:12
      ./periodic_fail_suite features/data/tmp-periodic_fail.feature:16
      ./periodic_fail_suite features/data/tmp-periodic_fail.feature:20

      4 scenarios (3 failed, 1 passed)
      20 steps (1 failed, 13 skipped, 6 passed)

      """


  @lang @lang-C
  Scenario: Periodic Fail
    When I run xreq -m -x periodic_fail_suite features/data/tmp-periodic_fail.feature
    Then it should pass
    When I run the test suite "./periodic_fail_suite" in features/data/tests
    Then it should fail with
      """
      Feature: Periodic Fail

        Background: Set things up
      This step works BACKGROUND
          Given this step works with BACKGROUND
      State is TRUE OK
          And this fails periodically
      This step works BACKGROUND
          And this step works with BACKGROUND

        Scenario: Run a good step 1
      This step works STEP
          Given this step works with STEP
          And this is ignored

        Scenario: Run a good step 2
      This step works BACKGROUND
          Given this fails periodically
            XREQLIB.C_INTERFACE.XREQ_FORMAT_PUT_ERROR.STEP_ERROR: State is FALSE (should be TRUE)
      in: ../step_definitions/simple_steps.c:37
          And this step works with BACKGROUND
          And this step works with STEP
          And this is ignored

        Scenario: Run a good step 3
      This step works BACKGROUND
      State is TRUE OK
      This step works BACKGROUND
      This step works STEP
          Given this step works with STEP
          And this is ignored

        Scenario: Run a good step 4
      This step works BACKGROUND
          Given this fails periodically
            XREQLIB.C_INTERFACE.XREQ_FORMAT_PUT_ERROR.STEP_ERROR: State is FALSE (should be TRUE)
      in: ../step_definitions/simple_steps.c:37
          And this step works with BACKGROUND
          And this step works with STEP
          And this is ignored

      ./periodic_fail_suite features/data/tmp-periodic_fail.feature:12
      ./periodic_fail_suite features/data/tmp-periodic_fail.feature:20

      4 scenarios (2 failed, 2 passed)
      20 steps (2 failed, 6 skipped, 12 passed)

      """

