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
      Error: Missing step definition in features/test.feature:4 for:
        Given a computer
      You can implement this step by adding on your step definition file:
        --  @given ^a computer$
        --  @todo

      Error: Missing step definition in features/test.feature:5 for:
        When I type on my keyboard "toto"
      You can implement this step by adding on your step definition file:
        --  @when ^I type on my keyboard "([^"]*)"$
        --  @todo

      Error: Missing step definition in features/test.feature:6 for:
        Then I should see "toto"
      You can implement this step by adding on your step definition file:
        --  @then ^I should see "([^"]*)"$
        --  @todo

      AdaSpec can create the procedures for you if you use --fill-steps
      """

  Scenario: Executing @todo steps
    Given a file "features/step_definitions/steps.ads":
      """
      package Steps is
        --  @given ^a computer$
        --  @when ^I type on my keyboard "([^"]*)"$
        --  @then ^I should see "([^"]*)"$
        --  @todo
      end Steps;
      """
    When I run adaspec -x suite features/test.feature
    Then it should pass
    When I compile "suite" in features/tests
    Then it should pass
    When I run "./suite" in features/tests
    Then it should fail
    And the output should contain
      """
      1 scenario (1 failed)
      3 steps (1 failed, 2 skipped)
      """
    And the output should contain
      """
      ADASPECLIB.NOT_YET_IMPLEMENTED
      """

  Scenario: Filling steps
    Given a file "features/step_definitions/steps.ads":
      """
      package Steps is
        --  @given ^a computer$
        --  @todo

        --  @when ^I type on my keyboard "([^"]*)"$
        --  @then ^I should see "([^"]*)"$
        --  @todo

        --  @when ^toto$
        --  @todo

        --  @then ^tata$
        --  @todo
      end Steps;
      """
    When I run adaspec --fill-steps features/test.feature
    Then it should pass
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
        --  @when ^I type on my keyboard "([^"]*)"$
        --  @then ^I should see "([^"]*)"$
        procedure Mixed_Step (Args : in out Arg_Type);
      """
    And "features/step_definitions/steps.ads" should contain
      """
        --  @when ^toto$
        procedure When_toto (Args : in out Arg_Type);
      """
    And "features/step_definitions/steps.ads" should contain
      """
        --  @then ^tata$
        procedure Then_tata (Args : in out Arg_Type);
      """
    And "features/step_definitions/steps.adb" should exist
    And "features/step_definitions/steps.adb" should contain
      """
      package body Steps is

         procedure Given_a_computer (Args : in out Arg_Type) is
         begin
      """
    And "features/step_definitions/steps.adb" should contain
      """
         end Given_a_computer;

         procedure Mixed_Step (Args : in out Arg_Type) is
         begin
      """
    And "features/step_definitions/steps.adb" should contain
      """
         end Mixed_Step;

         procedure When_toto (Args : in out Arg_Type) is
         begin
      """
    And "features/step_definitions/steps.adb" should contain
      """
         end When_toto;

         procedure Then_tata (Args : in out Arg_Type) is
         begin
      """
    And "features/step_definitions/steps.adb" should contain
      """
         end Then_tata;

      end Steps;
      """

  Scenario: Filling existing steps
    Given a file "features/step_definitions/steps.ads":
      """
      with AdaSpecLib;
      use  AdaSpecLib;
      package Steps is

         --  @given ^a computer$
         procedure Given_a_computer (Args : in out Arg_Type);

         --  @when ^I type on my keyboard "([^"]*)"$
         --  @then ^I should see "([^"]*)"$
         --  @todo

      end Steps;
      """
    Given a file "features/step_definitions/steps.adb":
      """
      package body Steps is

         procedure Given_a_computer (Args : in out Arg_Type) is
         begin
            raise AdaSpecLib.Not_Yet_Implemented
               with "Procedure not implemented: " & "Given_a_computer"
         end Given_a_computer;

      end Steps;
      """
    When I run adaspec --fill-steps -s features/step_definitions
    Then it should pass
    And "features/step_definitions/steps.ads" should contain
      """
         --  @when ^I type on my keyboard "([^"]*)"$
         --  @then ^I should see "([^"]*)"$
         procedure Mixed_Step (Args : in out Arg_Type);
      """
    And "features/step_definitions/steps.adb" should exist
    And "features/step_definitions/steps.adb" should contain
      """
      package body Steps is

         procedure Given_a_computer (Args : in out Arg_Type) is
         begin
            raise AdaSpecLib.Not_Yet_Implemented
               with "Procedure not implemented: " & "Given_a_computer"
         end Given_a_computer;

         procedure Mixed_Step (Args : in out Arg_Type) is
         begin
      """
    And "features/step_definitions/steps.adb" should contain
      """
         end Mixed_Step;

      end Steps;
      """
