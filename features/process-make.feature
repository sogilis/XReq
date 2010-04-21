Feature: Run gnatmake after compiling features
  In order to simplify command line invocations
  As a spec compiler
  I want XReq to run gnatmake after it generated the Ada source files

  Background:
    Given xreq is in the PATH
    And I am in the xreq directory
    Given a file "features/data/tmp-simplest.feature":
      """
      Feature: Sample

        Background:
          Given this step works loudly

        Scenario: Run a good step
          Given this step works loudly

      """

  @lang-Ada
  Scenario:
    When I run xreq -m -x suite features/data/tmp-simplest.feature
    Then it should pass
    And the output should contain
      """

      --> Success

      """
    And "features/data/tests/feature_tmp_simplest.ads" should exist
    And "features/data/tests/feature_tmp_simplest.adb" should exist
    And "features/data/tests/suite.adb" should exist
    And "features/data/tests/suite.gpr" should exist
    And "features/data/tests/suite" should exist

    When I run the test suite "features/data/tests/suite"
    Then it should pass with
      """
      Feature: Sample

        Background:
      This step works
          Given this step works loudly

        Scenario: Run a good step
      This step works
          Given this step works loudly

      1 scenario (1 passed)
      2 steps (2 passed)

      """

  @lang-Ada
  Scenario:
    When I run "GNAT_FLAGS=--non-existing-flag xreq -m -x suite features/data/tmp-simplest.feature"
    Then it should fail
    And the output should contain
      """
      --> Failure
      """