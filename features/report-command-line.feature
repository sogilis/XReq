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

      Background:
        Given this step works

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

  @wip
  Scenario: Help
    When I run "./test_suite -h"
    Then it should pass
    And the output should contain
      """
      test_suite - Run features

      SYNOPSIS

          test_suite [OPTIONS]
      """