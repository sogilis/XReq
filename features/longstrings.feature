Feature: Long strings
  In order to give more data to step definitions
  As a feature writer
  I want to be able to write multiline strings just like in Python

  Escape characters are documented in
  <http://www.python.org/doc/2.5.2/ref/strings.html>


  Background:
    Given adaspec is in the PATH
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
      """
    And   a file "features/step_definitions/steps.ads":
      """
      with Ada.Strings.Unbounded;
      with AdaSpecLib;
      use  Ada.Strings.Unbounded;
      use  AdaSpecLib;

      package Steps is
        First_String : Unbounded_String;
        Comparaison  : Boolean;

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
      package body Steps is

        procedure Given (Args : in out Arg_Type) is
        begin
          First_String := To_Unbounded_String (Args.Text);
        end Given;

        procedure Compare (Args : in out Arg_Type) is
          Origin  : constant String := To_String (First_String);
          Against : constant String := Args.Match (1);
          Esc     : Boolean := False;
          Offset  : Natural := 0;
          Char    : Character;
        begin
          Comparaison := True;
          My_Loop:
          for I in Against'Range loop
            if Esc then
              Esc := False;
              case Against (I) is
                when 'n'        => Char := ASCII.LF;
                when others     => Char := Against (I);
              end case;
              if Against (I) /= Origin (I - Offset) then
                Comparaison := False;
                exit My_Loop;
              end if;
            elif Against (I) = '\' then
              Esc = True;
              Offset := Offset + 1;
            elif Against (I) /= Origin (I - Offset) then
              Comparaison := False;
              exit My_Loop;
            end if;
          end loop;
        end Compare;

        procedure Equal (Args : in out Arg_Type) is
        begin
          Assert (Comparaison, "The strings are not equal");
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
          When I compare it with "abc\\ndef"
          Then they are equal

      1 scenarios (1 passed)
      3 steps (3 passed)

      """