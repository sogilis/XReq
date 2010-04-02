Feature: ambiguous step definition error reporting
  In order to avoid writing ambiguous step definitions
  As an adaspec user
  I want to be notified whenever the step definitions regular expressions are
  ambiguous for a given step

  Background:
    Given adaspec is in the PATH
    And I am in an empty directory
    And a file "features/ambiguous.feature":
      """
      Feature: Sample

        Background: B
          Given this step works

        Scenario: Run a bad step
          Given this step doesn't work
          Given this step works

        Scenario: Run a good step
          Given this step works

      """
    And a file "features/ambiguous2.feature":
      """
      Feature: Sample

        Scenario: Run an ambiguous step
          Given this is ambiguous

      """
    And a file "features/step_definitions/steps.ads":
      """
      with AdaSpecLib.General;
      use  AdaSpecLib.General;
      package Steps is

        --  @given ^this step works$
        procedure This_Step_Works (Args : in out Arg_Type);

        --  @given ^this is ambiguous$
        procedure Ambiguous (Args : in out Arg_Type);

        --  @given ^this is( not)? ambiguous$
        procedure Not_Ambiguous (Args : in out Arg_Type);

      end Steps;
      """
    And a file "features/step_definitions/steps2.ads":
      """
      with AdaSpecLib;
      use  AdaSpecLib;
      package Steps2 is

        --  @given ^this step .*works$
        procedure This_Step_Doest_Work (Args : in out Arg_Type);

      end Steps2;
      """

  Scenario:
    When I run adaspec features/ambiguous.feature
    Then it should fail
    And the output should contain
      """
      ERROR: Ambiguous match in features/ambiguous.feature:4 for:
        Given this step works

      """
    And the output should contain
      """
      ERROR: Ambiguous match in features/ambiguous.feature:8 for:
        Given this step works

      """
    And the output should contain
      """
      ERROR: Ambiguous match in features/ambiguous.feature:11 for:
        Given this step works

      """

    When I run adaspec -q features/ambiguous.feature
    Then it should fail
    And the output should contain
      """
      features/ambiguous.feature:4: ERROR: Ambiguous match for: Given this step works

      """
    And the output should contain
      """
      features/ambiguous.feature:8: ERROR: Ambiguous match for: Given this step works

      """
    And the output should contain
      """
      features/ambiguous.feature:11: ERROR: Ambiguous match for: Given this step works

      """

  Scenario:
    When I run adaspec features/ambiguous2.feature
    Then it should fail
    And the output should contain
      """
      ERROR: Ambiguous match in features/ambiguous2.feature:4 for:
        Given this is ambiguous

      """

    When I run adaspec -q features/ambiguous2.feature
    Then it should fail
    And the output should contain
      """
      features/ambiguous2.feature:4: ERROR: Ambiguous match for: Given this is ambiguous

      """

