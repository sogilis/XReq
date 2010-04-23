Feature: ambiguous step definition error reporting
  In order to avoid writing ambiguous step definitions
  As an xreq user
  I want to be notified whenever the step definitions regular expressions are
  ambiguous for a given step

  Background:
    Given xreq is in the PATH
    And I am in the xreq directory
    And a file "features/data/tmp-ambiguous.feature":
      """
      Feature: Sample

        Background: B
          Given this step is ambiguous

        Scenario: Run a bad step
          Given this step is too ambiguous
          Given this step is ambiguous

        Scenario: Run a good step
          Given this step is ambiguous

      """
    And a file "features/data/tmp-ambiguous2.feature":
      """
      Feature: Sample

        Scenario: Run an ambiguous step
          Given this is ambiguous too

      """

  Scenario: Ambiguity within the same step package
    When I run xreq features/data/tmp-ambiguous.feature
    Then it should fail
    And the output should contain
      """
      ERROR: Ambiguous match in features/data/tmp-ambiguous.feature:4 for:
        Given this step is ambiguous

      """
    And the output should contain
      """
      ERROR: Ambiguous match in features/data/tmp-ambiguous.feature:8 for:
        Given this step is ambiguous

      """
    And the output should contain
      """
      ERROR: Ambiguous match in features/data/tmp-ambiguous.feature:11 for:
        Given this step is ambiguous

      """

  Scenario: Ambiguity within the same step package - Quiet mode
    When I run xreq -q features/data/tmp-ambiguous.feature
    Then it should fail
    And the output should contain
      """
      features/data/tmp-ambiguous.feature:4: ERROR: Ambiguous match for: Given this step is ambiguous

      """
    And the output should contain
      """
      features/data/tmp-ambiguous.feature:8: ERROR: Ambiguous match for: Given this step is ambiguous

      """
    And the output should contain
      """
      features/data/tmp-ambiguous.feature:11: ERROR: Ambiguous match for: Given this step is ambiguous

      """

  Scenario: Ambiguity in two different packages
    When I run xreq features/data/tmp-ambiguous2.feature
    Then it should fail
    And the output should contain
      """
      ERROR: Ambiguous match in features/data/tmp-ambiguous2.feature:4 for:
        Given this is ambiguous too

      """

  Scenario: Ambiguity in two different packages - Quiet mode
    When I run xreq -q features/data/tmp-ambiguous2.feature
    Then it should fail
    And the output should contain
      """
      features/data/tmp-ambiguous2.feature:4: ERROR: Ambiguous match for: Given this is ambiguous too

      """

