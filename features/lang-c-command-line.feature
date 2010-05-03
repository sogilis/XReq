Feature: xreq commandline
  In order to execute xreq
  As an xreq user
  I want to have a command line interface

  Background:
    Given xreq is in the PATH
    And   I am in the xreq directory
    And   "features/data/tests" is empty

  @lang @lang-C
  Scenario: Step matching only (--partial --step-matching)
    When I run xreq -x suite -lC --partial --step-matching features/data/simplest.feature
    Then it should pass with
    """
    --> Compile: features/data/simplest.feature

    Load C steps in: features/data/step_definitions
    Compile: features/data/simplest.feature
    Step Matching: "features/data/simplest.feature:4" matches "features/data/step_definitions/simple_steps.h:14" procedure Given_this_step_works
    Step Matching: "features/data/simplest.feature:7" matches "features/data/step_definitions/simple_steps.h:14" procedure Given_this_step_works


    """
    And "features/data/tests/feature_simplest.c" should not exist
    And "features/data/tests/feature_simplest.d" should not exist
    And "features/data/tests/suite.c" should not exist
    And "features/data/tests/suite.mak" should not exist