Feature: Tags
  In order to document the features
  As an spec writer
  I want to be able to write comments

  Background:
    Given adaspec is in the PATH
    And I am in an empty directory
    Given a file "features/step_definitions/steps.ads":
      """
      package Steps is
        --  @given ^this step works$
        --  @todo
      end Steps;
      """

  Scenario:
    Given a file "features/comments.feature":
      """
      Feature: F

        # Comment

        Background:
          Given this step works

        # Comment

        Scenario: S
          Given this step works
      """
    When I run adaspec features/comments.feature
    Then it should pass