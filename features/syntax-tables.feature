Feature: Tables
  In order to check data on steps
  As an spec writer
  I want to write tables in my spec

  Background:
    Given xreq is in the PATH
    And I am in the xreq directory

  Scenario: Parse a table
    Given a file "features/data/tmp-table.feature":
      """
      Feature: F

        Scenario: S
          Given this step works
            | # | first comumn | second col | 3rd c. |
            | 1 |            1 |          2 |      3 |
            | 2 |            4 |          5 |      6 |
            | 3 |            7 |          8 |      9 |
            | 4 |           10 |         11 |     12 |
          Given this step works
      """
    When I run xreq features/data/tmp-table.feature
    Then it should pass

  Scenario: Compare two equal tables
    Given a file "features/data/tmp-equal_table.feature":
      """
      Feature: F

        Scenario: S
          Given a table:
            | a | b |
            | c | d |
            | e |
          Then the table should be equal to:
            | a | b |
            | c | d |
            | e |
      """
    When I run xreq -m -x equal_table features/data/tmp-equal_table.feature
    Then it should pass
    When I run the test suite "./equal_table" in features/data/tests
    Then it should pass with
      """
      Feature: F

        Scenario: S
          Given a table:
            | a | b |
            | c | d |
            | e |---|
          Then the table should be equal to:
            | a | b |
            | c | d |
            | e |---|

      1 scenario (1 passed)
      2 steps (2 passed)

      """
    When I run "./equal_table -f html -o equal_table.html" in features/data/tests
    Then it should pass
    And "features/data/tests/equal_table.html" should exist
    And "features/data/tests/equal_table.html" should contain
      """
                <table>
                  <tr>
                    <td>a</td>
                    <td>b</td>
                  </tr>
                  <tr>
                    <td>c</td>
                    <td>d</td>
                  </tr>
                  <tr>
                    <td>e</td>
                    <td></td>
                  </tr>
                </table>
      """
