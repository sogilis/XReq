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

        Background:
          Given two long strings:
            \"""
            a
            \"""
            \"""
            b
            \"""

        Scenario: A
          Given the long string:
            '''
            abc
              
            def
            '''
          When I compare it with "abc\n  \ndef"
          Then they are equal

        Scenario: B
          Given the long string:
            '''
            abc '''
            def \\'''
            '''
          When I compare it with "abc '''\ndef \\'''"
          Then they are equal
      """
    And   a file "features/step_definitions/steps.ads":
      """
      with Ada.Strings.Unbounded;
      with AdaSpecLib.General;
      use  Ada.Strings.Unbounded;
      use  AdaSpecLib.General;

      package Steps is
        First_String  : Unbounded_String;
        Second_String : Unbounded_String;
        Comparaison   : Boolean;

        --  @given ^two long strings:$
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
      with AdaSpecLib.Asserts;
      use  AdaSpecLib.Asserts;
      package body Steps is

        function Decode (Str : in String) return String is
          Buffer : Unbounded_String;
          I      : Natural := Str'First;
        begin
          while I <= Str'Last loop
            if Str (I) = '\' then
              I := I + 1;
              case Str (I) is
                when 'n' =>
                  Append (Buffer, ASCII.LF);
                when others =>
                  Append (Buffer, "\" & Str (I));
              end case;
            else
              Append (Buffer, Str (I));
            end if;
            I := I + 1;
          end loop;
          return To_String (Buffer);
        end Decode;

        procedure Given (Args : in out Arg_Type) is
        begin
          First_String := To_Unbounded_String (Args.Text);
        end Given;

        procedure Compare (Args : in out Arg_Type) is
          Origin  : constant String := To_String (First_String);
          Against : constant String := Decode (Args.Match (1));
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

        Background:
          Given two long strings:
            \"""
            a
            \"""
            \"""
            b
            \"""

        Scenario: A
          Given the long string:
            \"\"\"
            abc
              
            def
            \"\"\"
          When I compare it with "abc\n  \ndef"
          Then they are equal

        Scenario: B
          Given the long string:
            \"\"\"
            abc '''
            def \'''
            \"\"\"
          When I compare it with "abc '''\ndef \\'''"
          Then they are equal

      2 scenarios (2 passed)
      8 steps (8 passed)

      """

  Scenario: Test longstrings errors
    Given a file "features/longstring_error.feature":
      """
        Feature: Sample

          Scenario: A
            Given the long string:
              '''abc'''
      """
    When I run adaspec features/longstring_error.feature
    Then "features/tests/longstring_error.adb" should not exist
    Then "features/tests/longstring_error.ads" should not exist