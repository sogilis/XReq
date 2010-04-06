Feature: adaspec commandline
  In order to execute adaspec
  As an adaspec user
  I want to have a command line interface

  Background:
    Given adaspec is in the PATH
    And   I am in the adaspec directory
    When  I run "rm -f tests/features/tests/*"
    Then "tests/features/tests/feature_simplest.adb" should not exist
    And  "tests/features/tests/feature_simplest.ads" should not exist

  Scenario: Help message
    When I run adaspec -h
    Then it should pass
    And the output should contain
      """
      SYNOPSIS

          adaspec [OPTIONS] FEATURE ...

      """

  Scenario: Compile a simple feature
    When I run adaspec tests/features/simplest.feature
    Then it should pass
    And "tests/features/tests/feature_simplest.adb" should exist
    And "tests/features/tests/feature_simplest.ads" should exist

  Scenario: Compile a simple feature with --partial
    When I run adaspec -x suite --partial --step features/step_definitions tests/features/simplest.feature
    Then it should pass with
    """
    --> Compile: tests/features/simplest.feature

    Load Ada steps in: features/step_definitions
    Compile: tests/features/simplest.feature


    """
    And "tests/features/tests/feature_simplest.adb" should not exist
    And "tests/features/tests/feature_simplest.ads" should not exist
    And "tests/features/tests/suite.adb" should not exist
    And "tests/features/tests/suite.gpr" should not exist

  Scenario: Compile a simple feature with --partial --step-matching
    When I run adaspec -x suite --partial --step-matching --step features/step_definitions tests/features/simplest.feature
    Then it should pass with
    """
    --> Compile: tests/features/simplest.feature

    Load Ada steps in: features/step_definitions
    Compile: tests/features/simplest.feature
    Step Matching: "tests/features/simplest.feature:4" matches "features/step_definitions/sample1.ads:8" procedure Sample1.This_Step_Works
    Step Matching: "tests/features/simplest.feature:7" matches "features/step_definitions/sample1.ads:8" procedure Sample1.This_Step_Works


    """
    And "tests/features/tests/feature_simplest.adb" should not exist
    And "tests/features/tests/feature_simplest.ads" should not exist
    And "tests/features/tests/suite.adb" should not exist
    And "tests/features/tests/suite.gpr" should not exist

  Scenario: Choose a step directory
    When I run adaspec -otmp --step tmp tests/features/simplest.feature
    Then it should fail
    And the output should contain
      """
      Missing step definition
      """

  Scenario: Compile two features
    When I run adaspec tests/features/simplest.feature tests/features/simplest2.feature
    Then it should pass
    And "tests/features/tests/feature_simplest.adb" should exist
    And "tests/features/tests/feature_simplest.ads" should exist
    And "tests/features/tests/feature_simplest2.adb" should exist
    And "tests/features/tests/feature_simplest2.ads" should exist

  Scenario: Run adaspec with no features but specify output
    When I run adaspec -o tmp
    Then it should fail
    And the output should contain
      """
      Missing feature
      """

  Scenario: Run adaspec with no features but specify steps
    When I run adaspec --step=tmp
    Then it should fail
    And the output should contain
      """
      Missing feature
      """

  Scenario: Specify a wrong language
    When I run adaspec --lang toto tests/features/simplest.feature
    Then it should fail
    And the output should contain
      """
      Unknown language toto
      """

  Scenario: Don't specify an argument to a command line switch
    When I run adaspec --step
    Then it should fail
    And the output should contain
      """
      Missing parameter for switch --step
      """

  Scenario: Specify an unknown command line switch
    When I run adaspec --toto
    Then it should fail
    And the output should contain
      """
      Invalid switch
      """

  Scenario: Compile a feature with a defined language and step directory
    When I run adaspec --lang=ada --step tests/features/step_definitions tests/features/simplest.feature
    Then it should pass
    And "tests/features/tests/feature_simplest.adb" should exist
    And "tests/features/tests/feature_simplest.ads" should exist

  Scenario: Compile with two step directories
    When I run adaspec --step tmp --step tests/features/step_definitions tests/features/simplest.feature
    Then it should pass
    And "tests/features/tests/feature_simplest.adb" should exist
    And "tests/features/tests/feature_simplest.ads" should exist

  Scenario: Create an executable for all features
    When I run adaspec -x result1 -k tests/features/simplest.feature tests/features/simplest2.feature
    Then it should pass
    And "tests/features/tests/feature_simplest.adb" should exist
    And "tests/features/tests/feature_simplest.ads" should exist
    And "tests/features/tests/feature_simplest2.adb" should exist
    And "tests/features/tests/feature_simplest2.ads" should exist
    And "tests/features/tests/result1.adb" should exist
    When I compile "result1" in tests/features/tests
    Then it should pass
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
