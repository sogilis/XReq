Feature: Process
  In order to use AdaSpec
  As an AsaSpec user
  I want to be able to generate the ads and adb files for a test

  Background:
    Given an empty directory
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
      package Steps is

        --  @given ^this step works$
        procedure This_Step_Works;

      end Steps;
      """
    And a file "features/step_definitions/steps.adb":
      """
      with Ada.Text_IO;
      use Ada.Text_IO;
      package body Steps is

        procedure This_Step_Works is
        begin
          Put_Line ("This step works");
        end This_Step_Works;

      end Steps;
      """

  Scenario: simple
    When I run adaspec with "features/simplest.feature"
    Then it run successfully

