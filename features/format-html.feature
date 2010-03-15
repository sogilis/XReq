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

      Background:
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
    And a file "features/fail.feature":
      """
      Feature: Feature that fail

        Background: definitions
          Given it fail
            \"""
            This is a long string
            that take two lines

            \"""
          And this is ignored

        Scenario: A
          Given this step works
          And this is ignored
            \"""
            Another multi-line string
            \"""
          Then do nothing

        Scenario: B
          Given this step works
          And this is ignored
          Then do nothing
            \"""
            Another multi-line string
            \"""
      """
    And a file "features/pass.feature":
      """
      Feature: Feature that pass

        Background: definitions
          Given this step works
          And   match "this" and "that"
            \"""
            This is a long string
            that take two lines

            \"""
          And this is ignored

        Scenario: A
          Given this step works
          And this is ignored
            \"""
            Another multi-line string
            \"""
          Then do nothing

        Scenario: B
          Given this step works
            | # | My  | Pretty | Table   |
            | 1 | abc | def    | ghi jkl |
            | 2 | fgh | Tggb   | ljIGB J |
            | 3 | d   | khY    | Toto    |
            | 4 | 6ty | JKHG   | Tata    |
          And this is ignored
          Then do nothing
            \"""
            Another multi-line string
            \"""
      """
    And a file "features/pass2.feature":
      """
      Feature: Feature that pass (2)

        Background: definitions
          Given this step works
          And this is ignored
            \"""
            Sample
            \"""
            \"""
            <strong>
            Second
            &&
            String
            </strong>
            \"""

        Scenario:
          Given this step works
            \"""
            Another "multi-line" string
            \"""
          And this is ignored
          Then do nothing
      """
    And a file "features/step_definitions/steps.ads":
      """
      with AdaSpecLib.General;
      use  AdaSpecLib.General;
      package Steps is

        --  @given ^this step (works)$
        --  @given ^match "(.*)" and "(.*)"$
        procedure This_Step_Works (Args : in out Arg_Type);

        --  @given ^this (fails) periodically$
        procedure Periodic_Fail (Args : in out Arg_Type);

        --  @given ^it (fails?)$
        --  @when ^it (fails?)$
        procedure Make_It_Fail (Args : in out Arg_Type);

        --  @then ^do nothing$
        --  @given ^this is (ignored)$
        procedure Do_Nothing (Args : in out Arg_Type) is null;

      end Steps;
      """
    And a file "features/step_definitions/steps.adb":
      """
      with Ada.Text_IO;
      with AdaSpecLib.Asserts;
      use  Ada.Text_IO;
      use  AdaSpecLib.Asserts;
      package body Steps is

        Num : Positive := 2;

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
          Args.Add_Para ("Debug information:");
          Args.Add_Text ("Num =" & Num'Img & ASCII.LF &
                         "Success only when Num = 1");
          Assert (Num = 1, "Num =" & Num'Img & " /= 1");
        end Periodic_Fail;

        procedure Make_It_Fail (Args : in out Arg_Type) is
          pragma Unreferenced (Args);
        begin
          Assert (False, "Error message");
        end Make_It_Fail;

      end Steps;
      """

  Scenario: Generate HTML report that fail
    When I run adaspec -x test_suite_fail features/simplest.feature features/simplest2.feature features/fail.feature
    Then it should pass
    When I compile "test_suite_fail" in features/tests
    Then it should pass
    Given I am in "features/tests"
    Then "test_suite_fail" should exist
    When I run "./test_suite_fail -f html -o report-fail.html"
    Then it should fail
    And  "report-fail.html" should exist
    And  "report-fail.html" should contain
      """
      <html
      """
    When I run "cp report-fail.html ../../../reports/sample-html-fail.html"

  Scenario: Generate HTML report that pass
    When I run adaspec -x test_suite_pass features/pass.feature features/pass2.feature
    Then it should pass
    When I compile "test_suite_pass" in features/tests
    Then it should pass
    Given I am in "features/tests"
    Then "test_suite_pass" should exist
    When I run "./test_suite_pass -f html -o report-pass.html"
    Then it should pass
    And  "report-pass.html" should exist
    And  "report-pass.html" should contain
      """
      >Sample</pre>
      """
    When I run "cp report-pass.html ../../../reports/sample-html-pass.html"
