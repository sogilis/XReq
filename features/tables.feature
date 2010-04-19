Feature: Tables
  In order to check data on steps
  As an spec writer
  I want to write tables in my spec

  Background:
    Given xreq is in the PATH
    And I am in an empty directory
    Given a file "features/step_definitions/steps.ads":
      """
      with XReqLib.General;
      use  XReqLib.General;
      package Steps is
        --  @given ^this step works$
        --  @todo

        --  @given ^a table:$
        procedure Given_a_table (Args : in out Arg_Type);

        --  @then ^the table should be equal to:$
        procedure Then_the_table_should_be_equal_to (Args : in out Arg_Type);
      end Steps;
      """
    Given a file "features/step_definitions/steps.adb":
      """
      with XReqLib.Asserts;
      use  XReqLib.Asserts;
      package body Steps is

         T : Table_Type;

         procedure Given_a_table (Args : in out Arg_Type) is
         begin
            T := Args.Table;
         end Given_a_table;

         procedure Then_the_table_should_be_equal_to (Args : in out Arg_Type) is
         begin
            Assert (T = Args.Table);
         end Then_the_table_should_be_equal_to;

      end Steps;
      """

  Scenario: Parse a table
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
          Given this step works
      """
    When I run xreq features/table.feature
    Then it should pass

  Scenario: Compare two equal tables
    Given a file "features/equal_table.feature":
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
    When I run xreq -x equal_table features/equal_table.feature
    Then it should pass
    When I compile "equal_table" in features/tests
    Then it should pass
    When I run the test suite "./equal_table" in features/tests
    Then it should pass with
      """
      Feature: F

        Scenario: S
          Given a table:
            | a | b |
            | c | d |
            | e | |
          Then the table should be equal to:
            | a | b |
            | c | d |
            | e | |

      1 scenario (1 passed)
      2 steps (2 passed)

      """
    When I run "./equal_table -f html -o equal_table.html" in features/tests
    Then it should pass
    And "features/tests/equal_table.html" should exist
    And "features/tests/equal_table.html" should contain
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
