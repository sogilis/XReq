Feature: Scenario Outlines
  In order to write scenarios that can be customized with different values
  As a feature writer
  I want xreq to support scenario outlines

  Background:
    Given xreq is in the PATH
    And I am in the xreq directory

  Scenario: No outline
    Given a file "features/data/tmp-eating.feature":
      """
      Feature: eating

        Scenario: eating
          Given there are 12 cucumbers
          When I eat 5 cucumbers
          Then I should have 7 cucumbers

      """
    When I run xreq -m -x suite features/data/tmp-eating.feature
    Then it should pass
    When I run the test suite "./suite" in features/data/tests
    Then it should pass with
      """
      Feature: eating

        Scenario: eating
          Given there are 12 cucumbers
          When I eat 5 cucumbers
          Then I should have 7 cucumbers

      1 scenario (1 passed)
      3 steps (3 passed)

      """

  Scenario: Simple Scenario Outline
    Given a file "features/data/tmp-outline.feature":
      """
      Feature: eating

        Scenario Outline:
          Given there are <start> cucumbers

          Examples:
            | start |
            |  12   |

        Scenario Outline: eating
          Given there are <start> cucumbers
          When I eat <eat> cucumbers
          Then I should have <left> cucumbers

          Examples:
            | start | eat | left |
            |  12   |  5  |  7   |
            |  20   |  5  |  15  |

      """
    When I run xreq -m -x suite features/data/tmp-outline.feature
    Then it should pass

    When I run the test suite "./suite -f html -o report.html" in features/data/tests
    Then it should pass
    When I run "cp features/data/tests/report.html reports/sample-html-outline-1.html"
    Then it should pass

    When I run the test suite "./suite" in features/data/tests
    Then it should pass with
      """
      Feature: eating

        Scenario Outline:
          Given there are <start> cucumbers

          Examples:
            | start |
            | 12    |

        Scenario Outline: eating
          Given there are <start> cucumbers
          When I eat <eat> cucumbers
          Then I should have <left> cucumbers

          Examples:
            | start | eat | left |
            | 12    | 5   | 7    |
            | 20    | 5   | 15   |

      3 scenarios (3 passed)
      7 steps (7 passed)

      """

  @lang @lang-Ada
  Scenario: Failing Scenario Outline
    Given a file "features/data/tmp-outline2.feature":
      """
      Feature: eating

        @tag1 @tag2
        Scenario Outline: eating
          Given there are <start> cucumbers
          When I eat <eat> cucumbers
          Then I should have <left> cucumbers

          Examples:
            | start | eat | left |
            |  12   |  5  |  7   |
            |  20   |  5  |  NaN |

      """

    When I run xreq -m -x failing_suite features/data/tmp-outline2.feature
    Then it should fail
    And the output should contain
      """
      ERROR: Missing step definition in features/data/tmp-outline2.feature:7 for:
        Then I should have NaN cucumbers
      You can implement this step by adding on your step definition file:
        --  @then ^I should have NaN cucumbers$
        --  @todo
      """

    When I run xreq -m -x failing_suite -s features/data/step_definitions -s features/data/step_definitions2 features/data/tmp-outline2.feature
    Then it should pass

    When I run the test suite "./failing_suite -d -f html -o report.html" in features/data/tests
    Then it should fail
    When I run "cp features/data/tests/report.html reports/sample-html-outline-2.html"
    Then it should pass

    When I run the test suite "./failing_suite" in features/data/tests
    Then it should fail with
      """
      Feature: eating

        @tag1
        @tag2
        Scenario Outline: eating
          Given there are <start> cucumbers
          When I eat <eat> cucumbers
          Then I should have <left> cucumbers

          Scenario 2: eating
            Given there are 20 cucumbers
            When I eat 5 cucumbers
            Then I should have NaN cucumbers
              XREQLIB.NOT_YET_IMPLEMENTED: The step definition cound not be found

          Examples:
            | start | eat | left |
            | 12    | 5   | 7    |
            | 20    | 5   | NaN  |
      
      ./failing_suite features/data/tmp-outline2.feature:4

      2 scenarios (1 failed, 1 passed)
      6 steps (1 failed, 5 passed)

      """

  @lang @lang-C
  Scenario: Failing Scenario Outline
    Given a file "features/data/tmp-outline2.feature":
      """
      Feature: eating

        @tag1 @tag2
        Scenario Outline: eating
          Given there are <start> cucumbers
          When I eat <eat> cucumbers
          Then I should have <left> cucumbers

          Examples:
            | start | eat | left |
            |  12   |  5  |  7   |
            |  20   |  5  |  NaN |

      """

    When I run xreq -m -x failing_suite features/data/tmp-outline2.feature
    Then it should fail
    And the output should contain
      """
      ERROR: Missing step definition in features/data/tmp-outline2.feature:7 for:
        Then I should have NaN cucumbers
      You can implement this step by adding on your step definition file:
        --  @then ^I should have NaN cucumbers$
        --  @todo
      """

    When I run xreq -m -x failing_suite -s features/data/step_definitions -s features/data/step_definitions2 features/data/tmp-outline2.feature
    Then it should pass

    When I run the test suite "./failing_suite -d -f html -o report.html" in features/data/tests
    Then it should fail
    When I run "cp features/data/tests/report.html reports/sample-html-outline-2.html"
    Then it should pass

    When I run the test suite "./failing_suite" in features/data/tests
    Then it should fail with
      """
      Feature: eating

        @tag1
        @tag2
        Scenario Outline: eating
          Given there are <start> cucumbers
          When I eat <eat> cucumbers
          Then I should have <left> cucumbers

          Scenario 2: eating
            Then I should have NaN cucumbers
              XREQLIB.C_INTERFACE.XREQ_FORMAT_PUT_ERROR.STEP_ERROR: The step definition cound not be found

          Examples:
            | start | eat | left |
            |    12 |   5 |    7 |
            |    20 |   5 | NaN  |

      2 scenarios (1 failed, 1 passed)
      6 steps (1 failed, 5 passed)

      """
