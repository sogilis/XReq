Feature: Tags
  In order to run scenarios conditionally
  As an spec writer
  I want to specify tags to scenarios

  Background:
    Given adaspec is in the PATH
    And I am in an empty directory
    Given a file "features/step_definitions/steps.ads":
      """
      with AdaSpecLib.General;
      use  AdaSpecLib.General;
      package Steps is
         --  @given ^this step works$
         procedure Given_this_step_works (Args : in out Arg_Type);

         --  @given ^this step doesn't work$
         --  @todo
      end Steps;
      """
    Given a file "features/step_definitions/steps.adb":
      """
      package body Steps is

         procedure Given_this_step_works (Args : in out Arg_Type) is
            pragma Unreferenced (Args);
         begin
            null;
         end Given_this_step_works;

      end Steps;
      """

  Scenario: Show tags
    Given a file "features/tags.feature":
      """
      Feature: F

        @tagB

        Background:
          Given this step doesn't work

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
          Given this step doesn't work
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

  @wip
  Scenario: Conditional execution
    Given a file "features/conditional.feature":
      """
      Feature: Feature
        This is executed conditionnally

        @tagB1 @tagB2
        Background:
          Given this step works

        @tag1
        Scenario: S1
          Given this step works

        @tag2
        Scenario: S2
          Given this step works
      """
    When I run adaspec -x suite features/conditional.feature
    Then it should pass

    When I compile "suite" in features/tests
    Then it should pass

    When I run "./suite -t @tag1" in features/tests
    Then it should pass with
      """
      Feature: F

        @tagB1
        @tagB2
        Background:
          Given this step works

        @tag1
        Scenario: S1
          Given this step works

      1 scenario (1 passed)
      2 steps (2 passed)

      """

  @wip
  Scenario: Negative conditional execution
    Given a file "features/conditional.feature":
      """
      Feature: Feature
        This is executed conditionnally

        @tagB1 @tagB2
        Background:
          Given this step works

        @tag1
        Scenario: S1
          Given this step works

        @tag2
        Scenario: S2
          Given this step works
      """
    When I run adaspec -x suite features/conditional.feature
    Then it should pass

    When I compile "suite" in features/tests
    Then it should pass

    When I run "./suite -t ~@tag1" in features/tests
    Then it should pass with
      """
      Feature: F

        @tagB1
        @tagB2
        Background:
          Given this step works

        @tag2
        Scenario: S2
          Given this step works

      1 scenario (1 passed)
      2 steps (2 passed)

      """
