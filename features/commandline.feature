Feature: adaspec commandline
  In order to execute adaspec
  As an adaspec user
  I want to have a command line interface

  Background:
    Given adaspec is in the PATH
    And   I am in the adaspec directory

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
    And "tests/features/tests/simplest.adb" should exist
    And "tests/features/tests/simplest.ads" should exist

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
    And "tests/features/tests/simplest.adb" should exist
    And "tests/features/tests/simplest.ads" should exist
    And "tests/features/tests/simplest2.adb" should exist
    And "tests/features/tests/simplest2.ads" should exist

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
    And "tests/features/tests/simplest.adb" should exist
    And "tests/features/tests/simplest.ads" should exist

  @wip
  Scenario: Compile with two step directories
    When I run adaspec --step tmp --step tests/features/step_definitions tests/features/simplest.feature
    Then it should pass
    And "tests/features/tests/simplest.adb" should exist
    And "tests/features/tests/simplest.ads" should exist

  Scenario: Create an executable for all features
    When I run adaspec -x result1 -k tests/features/simplest.feature tests/features/simplest2.feature
    Then it should pass
    And "tests/features/tests/simplest.adb" should exist
    And "tests/features/tests/simplest.ads" should exist
    And "tests/features/tests/simplest2.adb" should exist
    And "tests/features/tests/simplest2.ads" should exist
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