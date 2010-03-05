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

      Background: Set things up
        Given this step works
          \"""
          abc
          def
          \"""

      Scenario: Run a good step
        Given this step works

      Scenario: Failure
        Given this step works
        When it fail
        Then do nothing

      """
    And a file "features/simplest2.feature":
      """
      Feature: Periodic Fail

      Background: Set things up
        Given this step works
          \"""
          abc
          def
          \"""
        And this fails periodically
        And this is ignored

      Scenario: Run a good step
        Given this step works
        And this is ignored

      Scenario: Run a good step
        Given this step works
        And this is ignored

      Scenario: Run a good step
        Given this step works
        And this is ignored

      Scenario: Run a good step
        Given this step works
        And this is ignored

      """
    And a file "features/step_definitions/steps.ads":
      """
      with AdaSpecLib;
      use  AdaSpecLib;
      package Steps is

        --  @given ^this step works$
        procedure This_Step_Works (Args : in out Arg_Type);

        --  @given ^this fails periodically$
        procedure Periodic_Fail (Args : in out Arg_Type);

        --  @when ^it fail$
        procedure Make_It_Fail (Args : in out Arg_Type);

        --  @then ^do nothing$
        --  @given ^this is ignored$
        procedure Do_Nothing (Args : in out Arg_Type) is null;

      end Steps;
      """
    And a file "features/step_definitions/steps.adb":
      """
      with Ada.Text_IO;
      use Ada.Text_IO;
      package body Steps is

        Num : Positive := 1;

        procedure This_Step_Works (Args : in out Arg_Type) is
          pragma Unreferenced (Args);
        begin
          Put_Line ("This step works");
        end This_Step_Works;

        procedure Periodic_Fail (Args : in out Arg_Type) is
          pragma Unreferenced (Args);
        begin
          Num := Num + 1;
          if Num = 3 then
            Num := 1;
          end if;
          Assert (Num = 1, "Num =" & Num'Img & " /= 1");
        end Periodic_Fail;

        procedure Make_It_Fail (Args : in out Arg_Type) is
          pragma Unreferenced (Args);
        begin
          Assert (False, "Error message");
        end Make_It_Fail;

      end Steps;
      """
    When I run adaspec -x test_suite features/simplest.feature features/simplest2.feature
    Then it should pass
    When I compile "test_suite" in features/tests
    Then it should pass
    Given I am in "features/tests"

  Scenario: Generate HTML report
    When I run "./test_suite -f html -o report.html"
    Then it should fail
    And  "report.html" should exist
    And  "report.html" should contain
      """
      <html
      """
