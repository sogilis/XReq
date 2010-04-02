Feature: Auto fill in of steps definitions
  In order to make writing steps easier
  As a step writer
  I want AdaSpec to automatically write a skeleton of step definitions for me.


  Background:
    Given adaspec is in the PATH
    And I am in an empty directory
    Given a file "features/test.feature":
      """
      Feature: TEST

        Scenario:
          Given a computer
          When I type on my keyboard "toto"
          Then I should see "toto"
      """

  Scenario: Reporting of missing steps
    When I run adaspec features/test.feature
    Then it should fail
    And the output should contain
      """
      ERROR: Missing step definition in features/test.feature:4 for:
        Given a computer
      You can implement this step by adding on your step definition file:
        --  @given ^a computer$
        --  @todo

      ERROR: Missing step definition in features/test.feature:5 for:
        When I type on my keyboard "toto"
      You can implement this step by adding on your step definition file:
        --  @when ^I type on my keyboard "([^"]*)"$
        --  @todo

      ERROR: Missing step definition in features/test.feature:6 for:
        Then I should see "toto"
      You can implement this step by adding on your step definition file:
        --  @then ^I should see "([^"]*)"$
        --  @todo

      AdaSpec can create the procedures for you if you use --fill-steps
      """

    When I run adaspec -q features/test.feature
    Then it should fail
    And the output should contain
      """
      features/test.feature:4: ERROR: Missing step definition for: Given a computer
      features/test.feature:5: ERROR: Missing step definition for: When I type on my keyboard "toto"
      features/test.feature:6: ERROR: Missing step definition for: Then I should see "toto"

      """
