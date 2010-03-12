Feature: Tables
  In order to check data on steps
  As an spec writer
  I want to write tables in my spec

  Background:
    Given adaspec is in the PATH
    And I am in an empty directory

  @wip
  Scenario:
    Given a file "features/table.feature":
      """
      Feature: F

        Scenario: S
          Given this step works
            | # | first comumn | second col | 3rd c. |
            | 1 |            1 |          2 |      3 |
            | 2 |            4 |          5 |      6 |
            | 3 |            7 |          8 |      9 |
            | 4 |           10 |         11 |     12 |
      """
    When I run adaspec features/table.feature
    Then it should pass