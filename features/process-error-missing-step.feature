Feature: Auto fill in of steps definitions
  In order to make writing steps easier
  As a step writer
  I want XReq to automatically write a skeleton of step definitions for me.


  Background:
    Given xreq is in the PATH
    And I am in the xreq directory
    Given a file "features/data/tmp-test.feature":
      """
      Feature: TEST

        Scenario:
          Given a computer
          When I type on my keyboard "toto"
          Then I should see "toto"
      """

  @lang @lang-Ada
  Scenario: Reporting of missing steps with Ada snippets
    When I run xreq features/data/tmp-test.feature
    Then it should fail
    And the output should contain
      """
      ERROR: Missing step definition in features/data/tmp-test.feature:4 for:
        Given a computer
      You can implement this step by adding on your step definition file:
        --  @given ^a computer$
        --  @todo

      ERROR: Missing step definition in features/data/tmp-test.feature:5 for:
        When I type on my keyboard "toto"
      You can implement this step by adding on your step definition file:
        --  @when ^I type on my keyboard "([^"]*)"$
        --  @todo

      ERROR: Missing step definition in features/data/tmp-test.feature:6 for:
        Then I should see "toto"
      You can implement this step by adding on your step definition file:
        --  @then ^I should see "([^"]*)"$
        --  @todo

      XReq can create the procedures for you if you use --fill-steps
      """

  Scenario: Reporting of missing steps in quiet mode
    When I run xreq -q features/data/tmp-test.feature
    Then it should fail
    And the output should contain
      """
      features/data/tmp-test.feature:4: ERROR: Missing step definition for: Given a computer
      features/data/tmp-test.feature:5: ERROR: Missing step definition for: When I type on my keyboard "toto"
      features/data/tmp-test.feature:6: ERROR: Missing step definition for: Then I should see "toto"

      """
