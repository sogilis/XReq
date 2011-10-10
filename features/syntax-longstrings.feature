Feature: Long strings
  In order to give more data to step definitions
  As a feature writer
  I want to be able to write multiline strings just like in Python

  Escape characters are documented in
  <http://www.python.org/doc/2.5.2/ref/strings.html>


  Background:
    Given xreq is in the PATH
    And   I am in the xreq directory
    And   a file "features/data/tmp-test.feature":
      """
      Feature: test

        Background:
          Given this step works
            \"\"\"
            a
            \"\"\"
            \"\"\"
            b
            \"\"\"

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

  Scenario: Test long strings
    When I run xreq -m -x suite features/data/tmp-test.feature
    Then it should pass
    When I run the test suite "./suite" in features/data/tests
    Then it should pass with
      """
      Feature: test

        Background:
          Given this step works
            \"\"\"
            a
            \"\"\"
            \"\"\"
            b
            \"\"\"

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
