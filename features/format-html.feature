Feature: HTML reports
  In order to have good looking reports
  As a person who checks reports
  I want to be able to have HTML reports

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

  Scenario: Generate HTML report
    When I run "./test_suite -f html -o report.html"
    Then it should pass
    And  "report.html" should exist
    And  "report.html" should contain
      """
      <html
      """
