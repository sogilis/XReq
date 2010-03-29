Feature: Run gnatmake after compiling features
  In order to simplify command line invocations
  As a spec compiler
  I want AdaSpec to run gnatmake after it generated the Ada source files

  Background:
    Given adaspec is in the PATH
    And I am in an empty directory

  @wip
  Scenario:
    Given a file "features/simplest.feature":
      """
      Feature: Sample

        Background:
          Given this step works

        Scenario: Run a good step
          Given this step works

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
    When I run adaspec -m -x suite features/simplest.feature
    Then it should pass
    And "features/tests/feature_simplest.ads" should exist
    And "features/tests/feature_simplest.adb" should exist
    And "features/tests/suite.adb" should exist
    And "features/tests/suite.gpr" should exist
    And "features/tests/suite" should exist

    When I run the test suite "features/tests/suite"
    Then it should pass with
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