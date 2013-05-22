Feature: Requirement Syntax
  In order to have a poduct adapted for the DO178B
  As an XReq end-user
  I want to be able to use the vocabilary of the DO178B instead of Cucumber
    Feature             -> Requirement
    Scenario            -> Test Case
    Scenario Outline    -> Test Case Template

  Background:
    Given xreq is in the PATH
    And I am in the xreq directory
    And a file "features/data/tmp-outline.requirement":
      """
      Requirement: eating

        Background:
          Given this step works

        Test Case:
          Given there are 5 cucumbers
          When I eat 3 cucumbers
          Then I should have 2 cucumbers

        Test Case Template: eating
          Given there are <start> cucumbers
          When I eat <eat> cucumbers
          Then I should have <left> cucumbers

          Examples:
            | start | eat | left |
            |  12   |  5  |  7   |
            |  20   |  5  |  15  |

      """

  Scenario: Syntax
    When I run xreq -m -x suite features/data/tmp-outline.requirement
    Then it should pass

    When I run the test suite "./suite" in features/data/tests
    Then it should pass with
      """
      Requirement: eating

        Background:
          Given this step works

        Test Case:
          Given there are 5 cucumbers
          When I eat 3 cucumbers
          Then I should have 2 cucumbers

        Test Case Template: eating
          Given there are <start> cucumbers
          When I eat <eat> cucumbers
          Then I should have <left> cucumbers

          Examples:
            | start | eat | left |
            | 12    | 5   | 7    |
            | 20    | 5   | 15   |

      3 scenarios (3 passed)
      12 steps (12 passed)

      """

  @lang @lang-Ada
  Scenario: Ada File Names
    When I run xreq -m features/data/tmp-outline.requirement
    Then it should pass
     And "features/data/tests/requirement_tmp_outline.adb" should exist
     And "features/data/tests/requirement_tmp_outline.ads" should exist

  @lang @lang-C
  Scenario: Ada File Names
    When I run xreq -m features/data/tmp-outline.requirement
    Then it should pass
     And "features/data/tests/requirement_tmp_outline.h" should exist
     And "features/data/tests/requirement_tmp_outline.c" should exist
