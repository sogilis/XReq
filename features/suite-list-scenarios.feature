Feature: List all scenarios compiled in a test suite
  In order to run scenarios conditionally based on their feature file name and
  scenario sequence number
  As an test suite runner
  I want to see the list of compiled scenarios in a test suite in the form of

  FEATURE_FILENAME:SCENARIO_NUM

  Background:
    Given xreq is in the PATH
    And I am in an empty directory
    Given a file "features/step_definitions/steps.ads":
      """
      with XReqLib.General;
      use  XReqLib.General;
      package Steps is
         --  @given ^this step works$
         procedure Given_this_step_works (Args : in out Arg_Type);

         --  @given ^this step doesn't work$
         --  @todo
      end Steps;
      """
    Given a file "features/step_definitions/steps.adb":
      """
      package body Steps is

         procedure Given_this_step_works (Args : in out Arg_Type) is
            pragma Unreferenced (Args);
         begin
            null;
         end Given_this_step_works;

      end Steps;
      """

  Scenario: Command line --list
    Given a file "features/a.feature":
      """
      Feature: A
        Description of A
        in two lines

        even three

        Background:
          Given this step works

        Scenario: S1
          Given this step works

        Scenario: S2
          Given this step works

        Scenario: S3
          Given this step works
      """
    Given a file "features/b.feature":
      """
      Feature: B
        Description of B
        in two lines

        even three

        Background:
          Given this step works

        Scenario: S1
          Given this step works

        Scenario: S2
          Given this step works

        Scenario: S3
          Given this step works
      """
    When I run xreq -x suite features/a.feature features/b.feature
    Then it should pass

    When I compile "suite" in features/tests
    Then it should pass

    When I run "./suite --list" in features/tests
    Then it should pass with
      """
      Feature: A

        features/a.feature:1 S1
        features/a.feature:2 S2
        features/a.feature:3 S3

      Feature: B

        features/b.feature:1 S1
        features/b.feature:2 S2
        features/b.feature:3 S3

      """