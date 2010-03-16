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

        @tagB

        Background:
          Given this step works

        @tag1 @tag2
        Scenario: S
          Given this step works
      """
    When I run adaspec -x suite features/tags.feature
    Then it should pass

    When I compile "suite" in features/tests
    Then it should pass

    When I run "./suite" in features/tests
    Then it should fail with
      """
      Feature: F

        @tagB
        Background:
          Given this step works
            ADASPECLIB.NOT_YET_IMPLEMENTED: The step definition cound not be found

        @tag1
        @tag2
        Scenario: S
          Given this step works

      1 scenario (1 failed)
      2 steps (1 failed, 1 skipped)

      """

    When I run "./suite -f html -o report.html" in features/tests
    Then it should fail
    And "features/tests/report.html" should exist
    And "features/tests/report.html" should contain
      """
      @tagB
      """
    And "features/tests/report.html" should contain
      """
      @tag1
      """
    And "features/tests/report.html" should contain
      """
      @tag2
      """
