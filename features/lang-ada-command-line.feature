Feature: xreq commandline
  In order to execute xreq
  As an xreq user
  I want to have a command line interface

  Background:
    Given xreq is in the PATH
    And   I am in the xreq directory
    When  I run "rm -f tests/features/tests/*"
    When  I run "rm -f features/data/tests/*"

  @lang @lang-Ada
  Scenario: Compile a simple feature
    When I run xreq tests/features/simplest.feature
    Then it should pass
    And "tests/features/tests/feature_simplest.adb" should exist
    And "tests/features/tests/feature_simplest.ads" should exist

  @lang @lang-Ada
  Scenario: Compile a simple feature with --partial
    When I run xreq -x suite --partial features/data/simplest.feature
    Then it should pass with
    """
    --> Compile: features/data/simplest.feature

    Load Ada steps in: features/data/step_definitions
    Compile: features/data/simplest.feature


    """
    And "features/data/tests/feature_simplest.adb" should not exist
    And "features/data/tests/feature_simplest.ads" should not exist
    And "features/data/tests/suite.adb" should not exist
    And "features/data/tests/suite.gpr" should not exist

  @lang @lang-Ada
  Scenario: Compile a simple feature with --partial --step-matching
    When I run xreq -x suite --partial --step-matching features/data/simplest.feature
    Then it should pass with
    """
    --> Compile: features/data/simplest.feature

    Load Ada steps in: features/data/step_definitions
    Compile: features/data/simplest.feature
    Step Matching: "features/data/simplest.feature:4" matches "features/data/step_definitions/simple_steps.ads:14" procedure Simple_Steps.Given_this_step_works
    Step Matching: "features/data/simplest.feature:7" matches "features/data/step_definitions/simple_steps.ads:14" procedure Simple_Steps.Given_this_step_works


    """
    And "features/data/tests/feature_simplest.adb" should not exist
    And "features/data/tests/feature_simplest.ads" should not exist
    And "features/data/tests/suite.adb" should not exist
    And "features/data/tests/suite.gpr" should not exist

  @lang @lang-Ada
  Scenario: Compile two features
    When I run xreq tests/features/simplest.feature tests/features/simplest2.feature
    Then it should pass
    And "tests/features/tests/feature_simplest.adb" should exist
    And "tests/features/tests/feature_simplest.ads" should exist
    And "tests/features/tests/feature_simplest2.adb" should exist
    And "tests/features/tests/feature_simplest2.ads" should exist

  @lang @lang-Ada
  Scenario: Compile a feature with a defined language and step directory
    When I run xreq --lang=ada --step tests/features/step_definitions tests/features/simplest.feature
    Then it should pass
    And "tests/features/tests/feature_simplest.adb" should exist
    And "tests/features/tests/feature_simplest.ads" should exist

  @lang @lang-Ada
  Scenario: Compile with two step directories
    When I run xreq --step tmp --step tests/features/step_definitions tests/features/simplest.feature
    Then it should pass
    And "tests/features/tests/feature_simplest.adb" should exist
    And "tests/features/tests/feature_simplest.ads" should exist

  @lang @lang-Ada
  Scenario: Create an executable for all features
    When I run xreq -m -x result1 -k tests/features/simplest.feature tests/features/simplest2.feature
    Then it should pass
    And "tests/features/tests/feature_simplest.adb" should exist
    And "tests/features/tests/feature_simplest.ads" should exist
    And "tests/features/tests/feature_simplest2.adb" should exist
    And "tests/features/tests/feature_simplest2.ads" should exist
    And "tests/features/tests/result1.adb" should exist
    And "tests/features/tests/result1" should exist
    When I run "tests/features/tests/result1"
    Then it should pass
    And the output should contain
      """
      Feature: Sample
      """
    And the output should contain
      """
      Feature: Sample2
      """
