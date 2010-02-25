Feature: Run steps with arguments
  In order to write more useful steps
  As an step writer
  I want to be able to capture the matches of the regular expressions

  Background:
    Given adaspec is in the PATH
    And   I am in an empty directory
    And   a file "features/test.feature":
      """
      Feature: test
        Scenario: A
          When I concatenate "abc" and "cde"
          Then I get "abccde"
      """
    And   a file "features/step_definitions/steps.ads":
      """
      with Ada.Strings.Unbounded;
      with AdaSpecLib;
      use  Ada.Strings.Unbounded;
      use  AdaSpecLib;
      package Steps is
        Result : Unbounded_String;
        --  @when ^I concatenate "(.*)" and "(.*)"$
        procedure Concat (Args : in out Arg_Type);
        --  @then ^I get "(.*)"$
        procedure I_Get (Args : in out Arg_Type);
      end Steps;
      """
    And   a file "features/step_definitions/steps.adb":
      """
      package body Steps is
        procedure Concat (Args : in out Arg_Type) is
        begin
          Result := To_Unbounded_String (Args.Match (1) & Args.Match (2));
        end Concat;
        procedure I_Get (Args : in out Arg_Type) is
        begin
          Assert (Args.Match (1) = To_string (Result));
        end I_Get;
      end Steps;
      """

  Scenario:
    When I run adaspec -x suite features/test.feature
    Then it should pass
    When I compile "suite" in features/tests
    Then it should pass
    When I run "./suite" in features/tests
    Then it should pass with
      """
      Feature: test

        Scenario: A
          When I concatenate "abc" and "cde"
          Then I get "abccde"

      """