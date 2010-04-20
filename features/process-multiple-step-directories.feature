Feature: Multiple step directories
  In order to be able to use different kind of step definitions
  As a spec writer
  I want to be able to use step definitions from different directories

  Background:
    Given xreq is in the PATH
    And I am in the xreq directory
    And a file "features/data/tmp-f.feature":
      """
      Feature: F

        Scenario: Run a bad step
          Given this step works
          And   this step works in another directory

      """

  Scenario:
    When I run xreq -x suite -m -s features/data/step_definitions1 --step features/data/step_definitions2 features/data/tmp-f.feature
    Then it should pass
