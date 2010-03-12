Feature: Tags
  In order to run scenarios conditionally
  As an spec writer
  I want to specify tags to scenarios

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
    Given a file "features/tags.feature":
      """
      Feature: F

        Background:
          Given this step works

        @tag
        Scenario: S
          Given this step works
      """
    When I run adaspec features/tags.feature
    Then it should pass