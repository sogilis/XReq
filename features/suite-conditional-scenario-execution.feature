Feature: Run conditionally scenarios
  In order to only run the scenarios I am interested in
  As a test suite runner
  I want to be able to specify on the command line the list of scenarios to
  execute in the form FILENAME:NUM


  Background:
    Given adaspec is in the PATH
    And I am in an empty directory
    Given a file "features/step_definitions/steps.ads":
      """
      with AdaSpecLib.General;
      use  AdaSpecLib.General;
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

  Scenario: Conditional run
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
    When I run adaspec -x suite features/a.feature features/b.feature
    Then it should pass

    When I compile "suite" in features/tests
    Then it should pass

    When I run "./suite features/a.feature:1 features/b.feature:3" in features/tests
    Then it should pass with
      """
      Feature: A
        Description of A
        in two lines
        
        even three

        Background:
          Given this step works

        Scenario: S1
          Given this step works

      Feature: B
        Description of B
        in two lines
        
        even three

        Background:
          Given this step works

        Scenario: S3
          Given this step works

      2 scenarios (2 passed)
      4 steps (4 passed)

      """

