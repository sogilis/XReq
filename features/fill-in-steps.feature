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

  @wip
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
        --  @given ^I type on my keyboard "([^"]*)"$
        --  @todo

      ERROR: Missing step definition in features/test.feature:6 for:
        Then I should see "toto"
      You can implement this step by adding on your step definition file:
        --  @given ^I should see "([^"]*)"$
        --  @todo

      AdaSpec can create the procedures for you if you use --fill-steps
      """

  @wip
  Scenario: Executing @todo steps
    Given a file "features/step_definitions/steps.ads":
      """
      package Steps is
        --  @given ^a computer$
        --  @given ^I type on my keyboard "([^"]*)"$
        --  @given ^I should see "([^"]*)"$
        --  @todo
      end Steps;
      """
    When I run adaspec -x suite features/test.feature
    Then it should pass
    When I compile "suite" in features/tests
    Then it should pass
    And the output should contain
      """
      1 scenario (2 failed)
      3 steps (1 failed, 2 skipped)
      """
    And the output should contain
      """
      ADASPECLIB.NOT_YET_IMPLEMENTED;
      """

  @wip
  Scenario: Filling steps
    Given a file "features/step_definitions/steps.ads":
      """
      package Steps is
        --  @given ^a computer$
        --  @todo

        --  @given ^I type on my keyboard "([^"]*)"$
        --  @given ^I should see "([^"]*)"$
        --  @todo
      end Steps;
      """
    When I run adaspec --fill-steps features/test.feature
    Then it should pass
    And "features/step_definitions/steps.adb" should exist
    And "features/step_definitions/steps.ads" should contain
      """
      with AdaSpecLib;
      use  AdaSpecLib;
      """
    And "features/step_definitions/steps.ads" should contain
      """
        --  @given ^a computer$
        procedure Given_a_computer (Args : in out Arg_Type);
      """
    And "features/step_definitions/steps.ads" should contain
      """
        --  @given ^I type on my keyboard "([^"]*)"$
        --  @given ^I should see "([^"]*)"$
        procedure Mixed_Step (Args : in out Arg_Type);
      """
    And "features/step_definitions/steps.adb" should contain
      """
      package body Steps is

        procedure Given_a_computer (Args : in out Arg_Type) is
        begin
          raise AdaSpecLib.Not_Yet_Implemented;
        end Given_a_computer;

        procedure Mixed_Step (Args : in out Arg_Type) is
        begin
          raise AdaSpecLib.Not_Yet_Implemented;
        end Mixed_Step;

      end Steps;
      """
