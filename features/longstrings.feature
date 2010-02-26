Feature: Long strings
  In order to give more data to step definitions
  As a feature writer
  I want to be able to write multiline strings just like in Python

  Escape characters are documented in
  <http://www.python.org/doc/2.5.2/ref/strings.html>


  Background:
    Given adaspec is in the PATH
    And   the sources of adaspec are in ADA_INCLUDE_PATH
    And   I am in an empty directory
    And   a file "features/test.feature":
      """
      Feature: test

        Scenario: A
          Given the long string:
            '''
            abc
            def
            '''
          When I compare it with "abc\ndef"
          Then they are equal

        Scenario: B
          Given the long string:
            \"\"\"abc
            def\"\"\"
          When I compare it with "abc\ndef"
          Then they are equal
      """
    And   a file "features/step_definitions/steps.ads":
      """
      with Ada.Strings.Unbounded;
      with AdaSpecLib;
      use  Ada.Strings.Unbounded;
      use  AdaSpecLib;

      package Steps is
        First_String  : Unbounded_String;
        Second_String : Unbounded_String;
        Comparaison   : Boolean;

        --  @given ^the long string:$
        procedure Given (Args : in out Arg_Type);

        --  @when ^I compare it with "(.*)"$
        procedure Compare (Args : in out Arg_Type);

        --  @then ^they are equal$
        procedure Equal (Args : in out Arg_Type);

      end Steps;
      """
    And   a file "features/step_definitions/steps.adb":
      """
      with Util.Strings;
      use  Util.Strings;
      package body Steps is

        procedure Given (Args : in out Arg_Type) is
        begin
          First_String := To_Unbounded_String (Args.Text);
        end Given;

        procedure Compare (Args : in out Arg_Type) is
          Origin  : constant String := To_String (First_String);
          Against : constant String := Decode_Python (Args.Match (1));
        begin
          Comparaison   := (Origin = Against);
          Second_String := To_Unbounded_String (Against);
        end Compare;

        procedure Equal (Args : in out Arg_Type) is
        begin
          Assert (Comparaison, "The strings are not equal: '" &
                  To_String (First_String) & "' /= '" & To_String (Second_String) & "'");
        end Equal;

      end Steps;
      """

  Scenario: Test long strings
    When I run adaspec -x suite features/test.feature
    Then it should pass
    When I compile "suite" in features/tests
    Then it should pass
    When I run "./suite" in features/tests
    Then it should pass with
      """
      Feature: test

        Scenario: A
          Given the long string:
            \"\"\"
            abc
            def
            \"\"\"
          When I compare it with "abc\ndef"
          Then they are equal

        Scenario: B
          Given the long string:
            \"\"\"
            abc
            def
            \"\"\"
          When I compare it with "abc\ndef"
          Then they are equal

      2 scenarios (2 passed)
      6 steps (6 passed)

      """