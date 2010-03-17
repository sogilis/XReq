Feature: Scenario Outlines
  In order to write scenarios that can be customized with different values
  As a feature writer
  I want adaspec to support scenario outlines

  Background:
    Given adaspec is in the PATH
    And I am in an empty directory
    Given a file "features/step_definitions/steps.ads":
      """
      with AdaSpecLib.General;
      use  AdaSpecLib.General;
      package Steps is
         --  @given ^there are ([0-9]+) cucumbers$
         procedure Given_there_are_n_cucumbers (Args : in out Arg_Type);

         --  @when ^I eat ([0-9]+) cucumbers$
         procedure When_i_eat_n_cucumbers (Args : in out Arg_Type);

         --  @then ^I should have ([0-9]+) cucumbers$
         procedure Then_i_should_have_n_cucumbers (Args : in out Arg_Type);

         --  @given ^this step doesn't work$
         --  @todo
      end Steps;
      """
    Given a file "features/step_definitions/steps.adb":
      """
      with AdaSpecLib.Asserts;
      use  AdaSpecLib.Asserts;
      package body Steps is

         Cukes : Integer := 0;

         procedure Given_there_are_n_cucumbers (Args : in out Arg_Type) is
         begin
            Cukes := Integer'Value (Args.Match (1));
         end Given_there_are_n_cucumbers;

         procedure When_i_eat_n_cucumbers (Args : in out Arg_Type) is
         begin
            Cukes := Cukes - Integer'Value (Args.Match (1));
         end When_i_eat_n_cucumbers;

         procedure Then_i_should_have_n_cucumbers (Args : in out Arg_Type) is
         begin
            Assert (Integer'Value (Args.Match (1)) = Cukes);
         end Then_i_should_have_n_cucumbers;

      end Steps;
      """

  Scenario: No outline
    Given a file "features/eating.feature":
      """
      Feature: eating

        Scenario: eating
          Given there are 12 cucumbers
          When I eat 5 cucumbers
          Then I should have 7 cucumbers

      """
    When I run adaspec -x suite features/eating.feature
    Then it should pass
    When I compile "suite" in features/tests
    Then it should pass
    When I run the test suite "./suite" in features/tests
    Then it should pass with
      """
      Feature: eating

        Scenario: eating
          Given there are 12 cucumbers
          When I eat 5 cucumbers
          Then I should have 7 cucumbers

      1 scenario (1 passed)
      3 steps (3 passed)

      """

  @wip
  Scenario: Simple Scenario Outline
    Given a file "features/outline.feature":
      """
      Feature: eating

        Scenario Outline: eating
          Given there are <start> cucumbers
          When I eat <eat> cucumbers
          Then I should have <left> cucumbers

          Examples:
            | start | eat | left |
            |  12   |  5  |  7   |
            |  20   |  5  |  15  |

      """
    And a file "features/step_definitions/steps.rb":
      """
      require 'test/unit/assertions'
      World(Test::Unit::Assertions)

      Given /^there are ([0-9]+) cucumbers$/ do |cukes|
        @cukes = cukes.to_i
      end

      When /^I eat ([0-9]+) cucumbers$/ do |cukes|
        @cukes = @cukes - cukes.to_i
      end

      Then /^I should have ([0-9]+) cucumbers$/ do |cukes|
        assert_equal (cukes.to_i, @cukes)
      end
      """
    When I run adaspec -x suite features/outline.feature
    Then it should pass
    When I compile "suite" in features/tests
    Then it should pass
    When I run the test suite "./suite" in features/tests
    Then it should pass with
      """
      Feature: eating

        Scenario Outline: eating
          Given there are <start> cucumbers
          When I eat <eat> cucumbers
          Then I should have <left> cucumbers

          Examples:
            | start | eat | left |
            |  12   |  5  |  7   |
            |  20   |  5  |  15  |

      2 scenario (2 passed)
      6 steps (6 passed)

      """
