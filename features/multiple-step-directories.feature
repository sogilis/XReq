Feature: Multiple step directories
  In order to be able to use different kind of step definitions
  As a spec writer
  I want to be able to use step definitions from different directories

  Background:
    Given xreq is in the PATH
    And I am in an empty directory
    And a file "features/f.feature":
      """
      Feature: F

        Scenario: Run a bad step
          Given this step works
          And   this step works too

      """
    And a file "features/step_definitions/steps.ads":
      """
      package Steps is

        --  @given ^this step works$
        --  @todo
        --  Ambiguous

      end Steps;
      """
    And a file "features/steps1/steps.ads":
      """
      with XReqLib.General;
      use  XReqLib.General;
      package Steps is

         --  @given ^this step works$
         procedure Given_this_step_works (Args : in out Arg_Type);

      end Steps;
      """
    And a file "features/steps1/steps.adb":
      """
      package body Steps is

         --  @given ^this step works too$
         procedure Given_this_step_works (Args : in out Arg_Type) is
         begin
            null;
         end Given_this_step_works;

      end Steps;
      """
    And a file "features/steps2/steps2.ads":
      """
      with XReqLib.General;
      use  XReqLib.General;
      package Steps2 is

         --  @given ^this step works too$
         procedure Given_this_step_works_too (Args : in out Arg_Type);

      end Steps2;
      """
    And a file "features/steps2/steps2.adb":
      """
      package body Steps2 is

         --  @given ^this step works_too$
         procedure Given_this_step_works_too (Args : in out Arg_Type) is
         begin
            null;
         end Given_this_step_works_too;

      end Steps2;
      """

  Scenario:
    When I run xreq -x suite -m -s features/steps1 --step features/steps2 features/f.feature
    Then it should pass
