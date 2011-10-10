Feature: Parsing Errors
  In order to write correct features
  As an spec writer
  I want to be notified of syntax errors in features

  Background:
    Given xreq is in the PATH
    And I am in the xreq directory

  Scenario: And keyword at beginning
    Given a file "features/data/tmp-error-and.feature":
      """
      Feature: Sample

        Background: B
          And this is a step
            \"\"\"
            With a long string
            \"\"\"

      """
    When I run xreq features/data/tmp-error-and.feature
    Then it should pass
    And the output should contain
      """
      ERROR: And keyword in features/data/tmp-error-and.feature line 4
             And keyword should be following another keyword
             Ignoring step
      """

    When I run xreq -q features/data/tmp-error-and.feature
    Then it should pass
    And the output should contain
      """
      features/data/tmp-error-and.feature:4: ERROR: And keyword should be following another keyword
      features/data/tmp-error-and.feature:4: ERROR: Ignoring step

      """


  Scenario: Invalid step
    Given a file "features/data/tmp-error-and.feature":
      """
      Feature: Sample

        Background: B
            stray line
          Given this step works
          Then use it

      """
    When I run xreq features/data/tmp-error-and.feature
    Then it should fail
    And the output should contain
      """
      ERROR: invalid format in features/data/tmp-error-and.feature line 4

      """

    When I run xreq -q features/data/tmp-error-and.feature
    Then it should fail
    And the output should contain
      """
      features/data/tmp-error-and.feature:4: ERROR: invalid format

      """


  Scenario: Invalid step (2)
    Given a file "features/data/tmp-error-and.feature":
      """
      Feature: Sample

        Background: B
          Given this step works
            stray line
          Then use it

      """
    When I run xreq features/data/tmp-error-and.feature
    Then it should fail
    And the output should contain
      """
      ERROR: invalid format in features/data/tmp-error-and.feature line 5

      """

    When I run xreq -q features/data/tmp-error-and.feature
    Then it should fail
    And the output should contain
      """
      features/data/tmp-error-and.feature:5: ERROR: invalid format

      """

  Scenario: Stray characters in long string
    Given a file "features/data/tmp-longstring.feature":
      """
      Feature: Sample

        Background: B
          Given this step works
            \"\"\"with stray characters
            With a long string
            \"\"\"

      """

    When I run xreq features/data/tmp-longstring.feature
    Then it should pass
    And the output should contain
      """
      ERROR: stray characers after \"\"\" in features/data/tmp-longstring.feature line 5

      """

    When I run xreq -q features/data/tmp-longstring.feature
    Then it should pass
    And the output should contain
      """
      features/data/tmp-longstring.feature:5: ERROR: stray characers after \"\"\"

      """


  Scenario: Error in long strings
    Given a file "features/data/tmp-longstring2.feature":
      """
        Feature: Sample

          Scenario: A
            Given the long string:
              '''abc'''
      """

    When I run xreq features/data/tmp-longstring2.feature
    Then the output should contain
      """
      ERROR: stray characers after ''' in features/data/tmp-longstring2.feature line 5

      """


    When I run xreq -q features/data/tmp-longstring2.feature
    Then the output should contain
      """
      features/data/tmp-longstring2.feature:5: ERROR: stray characers after '''

      """
