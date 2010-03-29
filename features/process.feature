Feature: The simplest works
  In order to use AdaSpec
  As an AsaSpec user
  I want to be able to generate the ads and adb files for a test and the test
  suite

  Background:
    Given adaspec is in the PATH
    And I am in an empty directory
    And a file "features/simplest.feature":
      """
      Feature: Sample
      Description Line 1
      Description Line 2
      Description Line 3

      Background:
        Given this step works

      Scenario: Run a good step
        Given this step works

      """
    And a file "features/empty.feature":
      """
      Feature: Empty

        Scenario: Empty
          # empty


      """
    And a file "features/step_definitions/steps.ads":
      """
      with AdaSpecLib.General;
      use  AdaSpecLib.General;
      package Steps is

        --  @given ^this step works$
        procedure This_Step_Works (Args : in out Arg_Type);

      end Steps;
      """
    And a file "features/step_definitions/steps.adb":
      """
      with Ada.Text_IO;
      use Ada.Text_IO;
      package body Steps is

        procedure This_Step_Works (Args : in out Arg_Type) is
          pragma Unreferenced (Args);
        begin
          Put_Line ("This step works");
        end This_Step_Works;

      end Steps;
      """

  Scenario: Test the generation of test packages
    When I run adaspec --progress features/simplest.feature
    Then it should pass with
      """
      --> Compile: features/simplest.feature

      Load Ada steps in: features/step_definitions
      Compile: features/simplest.feature
      Generate: features/tests/feature_simplest.ads
      Generate: features/tests/feature_simplest.adb
      completed 1 out of 1


      """
    And  "features/tests/feature_simplest.ads" should exist
    And  "features/tests/feature_simplest.adb" should exist
    When I compile "feature_simplest" in features/tests
    Then it should pass


  Scenario: Test the generation of the test suite
    When  I run adaspec -x simplest_test features/simplest.feature
    Then  it should pass
    And   "features/tests/feature_simplest.ads" should exist
    And   "features/tests/feature_simplest.adb" should exist
    And   "features/tests/simplest_test.adb" should exist
    And   "features/tests/simplest_test.gpr" should exist
    When  I compile "simplest_test" in features/tests
    Then  it should pass
    And   "features/tests/simplest_test" should exist
    When  I run the test suite "./simplest_test" in features/tests
    Then  it should pass with
      """
      Feature: Sample
        Description Line 1
        Description Line 2
        Description Line 3

        Background:
      This step works
          Given this step works

        Scenario: Run a good step
      This step works
          Given this step works

      1 scenario (1 passed)
      2 steps (2 passed)

      """


  Scenario: Test the generation of the test suite with a project file
    When  I run adaspec -x suite features/simplest.feature
    Then  it should pass
    And   "features/tests/feature_simplest.ads" should exist
    And   "features/tests/feature_simplest.adb" should exist
    And   "features/tests/suite.adb" should exist
    And   "features/tests/suite.gpr" should exist
    When  I run "gnatmake -Psuite.gpr $GNAT_FLAGS" in features/tests
    Then  it should pass
    And   "features/tests/suite" should exist
    When  I run the test suite "./suite" in features/tests
    Then  it should pass with
      """
      Feature: Sample
        Description Line 1
        Description Line 2
        Description Line 3

        Background:
      This step works
          Given this step works

        Scenario: Run a good step
      This step works
          Given this step works

      1 scenario (1 passed)
      2 steps (2 passed)

      """

  Scenario: Test an empty feature
    When I run adaspec -x empty_suite features/empty.feature
    Then it should pass
    When I compile "empty_suite" in features/tests
    Then it should pass
    When I run the test suite "./empty_suite" in features/tests
    Then it should pass with
      """
      Feature: Empty

        Scenario: Empty

      1 scenario (1 passed)
      0 steps

      """

