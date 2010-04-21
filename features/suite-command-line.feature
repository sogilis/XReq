Feature: Generated Test Suite Command Line
  In order to be able to generate different report formats
  As an xreq user
  I want to have a command line interface for the reports generated with
  xreq -x

  Background:
    Given xreq is in the PATH
    And I am in the xreq directory
    And a file "features/data/tmp-simplest.feature":
      """
      Feature: Sample

      Scenario: Run a good step
        Given this step works loudly

      """
    When I run xreq -m -x test_suite features/data/tmp-simplest.feature
    Then it should pass
    Given I am in "features/data/tests"
    Then "test_suite" should exist

  Scenario: Help
    When I run the test suite "./test_suite -h"
    Then it should pass
    And the output should contain
      """
      test_suite - Run features

      SYNOPSIS

          test_suite [OPTIONS]
      """

  Scenario: Text Format
    When I run the test suite "./test_suite -f TexT"
    Then it should pass
    And the output should contain
      """
      1 scenario (1 passed)
      1 step (1 passed)
      """

  Scenario: Unknown Format
    When I run the test suite "./test_suite -f toto"
    Then it should pass
    And the output should contain
      """
      Invalid format: toto
      """
    And the output should contain
      """
      1 scenario (1 passed)
      1 step (1 passed)
      """

  Scenario: Unknown Switch
    When I run "./test_suite --toto"
    Then it should fail
    And the output should contain
    """
    Invalid switch
    """

  Scenario: Missing parameter
    When I run "./test_suite --format"
    Then it should fail
    And the output should contain
    """
    Missing parameter for switch --format
    """

  Scenario: Text with output
    When I run the test suite "./test_suite -f TexT -o report.txt"
    Then it should pass
    And  "report.txt" should exist
    And  "report.txt" should contain
      """
      1 scenario (1 passed)
      1 step (1 passed)
      """

  Scenario: Text with debug flag
    When I run the test suite "./test_suite -f TexT -d"
    Then it should pass
    And  the output should contain
      """
      Debug text
      """