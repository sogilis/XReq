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
      features/data/tmp-ambiguous.feature:4: ERROR: Ambiguous match for: Given this step is ambiguous
      features/data/tmp-ambiguous.feature:4: ERROR: with: features/data/step_definitions/simple_steps.ads:6: Simple_Steps.Given_this_step_works
      features/data/tmp-ambiguous.feature:4: ERROR: and:  features/data/step_definitions/simple_steps.ads:7: Simple_Steps.Given_this_step_works

      """
    And the output should contain
      """
      features/data/tmp-ambiguous.feature:8: ERROR: Ambiguous match for: Given this step is ambiguous
      features/data/tmp-ambiguous.feature:8: ERROR: with: features/data/step_definitions/simple_steps.ads:6: Simple_Steps.Given_this_step_works
      features/data/tmp-ambiguous.feature:8: ERROR: and:  features/data/step_definitions/simple_steps.ads:7: Simple_Steps.Given_this_step_works

      """
    And the output should contain
      """
      features/data/tmp-ambiguous.feature:11: ERROR: Ambiguous match for: Given this step is ambiguous
      features/data/tmp-ambiguous.feature:11: ERROR: with: features/data/step_definitions/simple_steps.ads:6: Simple_Steps.Given_this_step_works
      features/data/tmp-ambiguous.feature:11: ERROR: and:  features/data/step_definitions/simple_steps.ads:7: Simple_Steps.Given_this_step_works

      """

  Scenario: Ambiguity in two different packages
    When I run xreq features/data/tmp-ambiguous2.feature
    Then it should fail
    And the output should contain
      """
      features/data/tmp-ambiguous2.feature:4: ERROR: Ambiguous match for: Given this is ambiguous too
      features/data/tmp-ambiguous2.feature:4: ERROR: with: features/data/step_definitions/ambiguous_steps.ads:6: 
      features/data/tmp-ambiguous2.feature:4: ERROR: and:  features/data/step_definitions/simple_steps.ads:8: Simple_Steps.Given_this_step_works

      """

  Scenario: Ambiguity in two different packages - Quiet mode
    When I run xreq -q features/data/tmp-ambiguous2.feature
    Then it should fail
    And the output should contain
      """
      features/data/tmp-ambiguous2.feature:4: ERROR: Ambiguous match for: Given this is ambiguous too

      """

