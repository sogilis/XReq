Feature: Parsing Errors
  In order to write correct features
  As an spec writer
  I want to be notified of syntax errors in features

  Background:
    Given adaspec is in the PATH
    And I am in an empty directory

  Scenario: And keyword at beginning
    Given a file "features/error-and.feature":
      """
      Feature: Sample

        Background: B
          And this is a step
            \"""
            With a long string
            \"""

      """
    When I run adaspec features/error-and.feature
    Then it should pass
    And the output should contain
      """
      ERROR: And keyword in features/error-and.feature line 4
             And keyword should be following another keyword
             Ignoring step
      """

    When I run adaspec -q features/error-and.feature
    Then it should pass
    And the output should contain
      """
      features/error-and.feature:4: ERROR: And keyword should be following another keyword
      features/error-and.feature:4: ERROR: Ignoring step

      """


  Scenario: Invalid step
    Given a file "features/error-and.feature":
      """
      Feature: Sample

        Background: B
            stray line
          Given this step works
          Then use it

      """
    When I run adaspec features/error-and.feature
    Then it should fail
    And the output should contain
      """
      ERROR: invalid format in features/error-and.feature line 4

      """

    When I run adaspec -q features/error-and.feature
    Then it should fail
    And the output should contain
      """
      features/error-and.feature:4: ERROR: invalid format

      """


  Scenario: Invalid step (2)
    Given a file "features/error-and.feature":
      """
      Feature: Sample

        Background: B
          Given this step works
            stray line
          Then use it

      """
    When I run adaspec features/error-and.feature
    Then it should fail
    And the output should contain
      """
      ERROR: invalid format in features/error-and.feature line 5

      """

    When I run adaspec -q features/error-and.feature
    Then it should fail
    And the output should contain
      """
      features/error-and.feature:5: ERROR: invalid format

      """

  Scenario: Stray characters in long string
    Given a file "features/longstring.feature":
      """
      Feature: Sample

        Background: B
          Given this is a step
            \"""with stray characters
            With a long string
            \"""

      """
    And a file "features/step_definitions/steps.ads":
      """
      package Steps is
         --  @given ^this is a step$
         --  @todo
      end Steps;
      """

    When I run adaspec features/longstring.feature
    Then it should pass
    And the output should contain
      """
      ERROR: stray characers after """ in features/longstring.feature line 5

      """

    When I run adaspec -q features/longstring.feature
    Then it should pass
    And the output should contain
      """
      features/longstring.feature:5: ERROR: stray characers after """

      """
