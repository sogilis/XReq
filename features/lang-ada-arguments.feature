Feature: Run steps with arguments (captures)
  In order to write more useful steps
  As an step writer
  I want to be able to capture the matches of the regular expressions

  Background:
    Given xreq is in the PATH
    And   I am in an empty directory

  @lang @lang-Ada
  Scenario: Test concatenation using string captures
    Given a file "features/test.feature":
      """
      Feature: test
        Scenario: A
          When I concatenate "abc" and "cde"
          Then I get "abccde"
      """
    And   a file "features/step_definitions/steps.ads":
      """
      with Ada.Strings.Unbounded;
      with XReqLib.General;
      use  Ada.Strings.Unbounded;
      use  XReqLib.General;
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
      with XReqLib.Asserts;
      use  XReqLib.Asserts;
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
    When I run xreq -m -x suite features/test.feature
    Then it should pass
    When I run the test suite "./suite" in features/tests
    Then it should pass with
      """
      Feature: test

        Scenario: A
          When I concatenate "abc" and "cde"
          Then I get "abccde"

      1 scenario (1 passed)
      2 steps (2 passed)

      """