Feature:
  In order to be able to generate different report formats
  As an adaspec user
  I want to have a command line interface for the reports generated with
  adaspec -x

  Background:
    Given adaspec is in the PATH
    And I am in an empty directory
    And a file "features/simplest.feature":
      """
      Feature: Sample

      Scenario: Run a good step
        Given this step works

      """
    And a file "features/step_definitions/steps.ads":
      """
      with AdaSpecLib;
      use  AdaSpecLib;
      package Steps is

        --  @given ^this step works$
        procedure This_Step_Works (Args : in out Arg_Type);

      end Steps;
      """
    And a file "features/step_definitions/steps.adb":
      """
      with Ada.Text_IO;
      use Ada.Text_IO;
      package body Steps is

        procedure This_Step_Works (Args : in out Arg_Type) is
          pragma Unreferenced (Args);
        begin
          Put_Line ("This step works");
        end This_Step_Works;

      end Steps;
      """
    When I run adaspec -x test_suite features/simplest.feature
    Then it should pass
    When I compile "test_suite" in features/tests
    Then it should pass
    Given I am in "features/tests"

  Scenario: Help
    When I run "./test_suite -h"
    Then it should pass
    And the output should contain
      """
      test_suite - Run features

      SYNOPSIS

          test_suite [OPTIONS]
      """

  Scenario: Text Format
    When I run "./test_suite -f TexT"
    Then it should pass
    And the output should contain
      """
      1 scenario (1 passed)
      1 step (1 passed)
      """

  Scenario: Unknown Format
    When I run "./test_suite -f toto 2>&1"
    Then it should pass
    And the output should contain
      """
      Invalid format: toto
      """
    And the output should contain
      """
      1 scenario (1 passed)
      1 step (1 passed)
      """

  Scenario: Unknown Switch
    When I run "./test_suite -toto 2>&1"
    Then it should fail
    And the output should contain
    """
    Invalid switch
    """

  Scenario: Missing parameter
    When I run "./test_suite --format 2>&1"
    Then it should fail
    And the output should contain
    """
    Missing parameter for switch --format
    """

  @wip
  Scenario: Text with output
    When I run "./test_suite -f TexT -o report.txt"
    Then it should pass
    And  "report.txt" should exist
    And  "report.txt" should contain
      """
      1 scenario (1 passed)
      1 step (1 passed)
      """