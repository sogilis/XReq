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

      Background:
        Given this step works

      Scenario: Run a good step
        Given this step works

      """
    And a file "features/step_definitions/steps.ads":
      """
      with AdaSpecLib;
      use  AdaSpecLib;
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
    When I run adaspec features/simplest.feature
    Then it should pass
    And  "features/tests/simplest.ads" should exist
    And  "features/tests/simplest.adb" should exist
    When I compile "simplest" in features/tests
    Then it should pass


  Scenario: Test the generation of the test suite
    When  I run adaspec -x simplest_test features/simplest.feature
    Then  it should pass
    And   "features/tests/simplest.ads" should exist
    And   "features/tests/simplest.adb" should exist
    And   "features/tests/simplest_test.adb" should exist
    When  I compile "simplest_test" in features/tests
    Then  it should pass
    And   "features/tests/simplest_test" should exist
    When  I run "./simplest_test" in features/tests
    Then  it should pass with
      """
      Feature: Sample

        Background:
      This step works
          Given this step works

        Scenario: Run a good step
      This step works
          Given this step works

      1 scenario (1 passed)
      2 steps (2 passed)

      """

