Feature: Auto fill in of steps definitions
  In order to make writing steps easier
  As a step writer
  I want XReq to automatically write a skeleton of step definitions for me.


  Background:
    Given xreq is in the PATH
    And I am in an empty directory
    Given a file "features/test.feature":
      """
      Feature: TEST

        Scenario:
          Given a computer
          When I type on my keyboard "toto"
          Then I should see "toto"
      """

  @lang @lang-Ada
  Scenario: Executing @todo steps
    Given a file "features/step_definitions/steps.ads":
      """
      package Steps is
        --  @given ^a computer$
        --  @when ^I type on my keyboard "([^"]*)"$
        --  @then ^I should see "([^"]*)"$
        --  @todo
      end Steps;
      """
    When I run xreq -m -x suite features/test.feature
    Then it should pass
    When I run "./suite --no-color" in features/tests
    Then it should fail
    And the output should contain
      """
      1 scenario (1 failed)
      3 steps (1 failed, 2 skipped)
      """
    And the output should contain
      """
      XREQLIB.NOT_YET_IMPLEMENTED
      """

  @lang @lang-Ada
  Scenario: Filling steps
    Given a file "features/step_definitions/steps.ads":
      """
      package Steps is
        --  @given ^a computer$
        --  @todo

        --  @when ^I type on my keyboard "([^"]*)"$
        --  @then ^I should see "([^"]*)"$
        --  @todo

        --  @when ^toto$
        --  @todo

        --  @then ^tata$
        --  @todo
      end Steps;
      """
    When I run xreq --fill-steps features/test.feature
    Then it should pass
    And "features/step_definitions/steps.ads" should contain
      """
      with XReqLib.General;
      use  XReqLib.General;
      """
    And "features/step_definitions/steps.ads" should contain
      """
        --  @given ^a computer$
        procedure Given_a_computer (Args : in out Arg_Type);
      """
    And "features/step_definitions/steps.ads" should contain
      """
        --  @when ^I type on my keyboard "([^"]*)"$
        --  @then ^I should see "([^"]*)"$
        procedure Mixed_Step (Args : in out Arg_Type);
      """
    And "features/step_definitions/steps.ads" should contain
      """
        --  @when ^toto$
        procedure When_toto (Args : in out Arg_Type);
      """
    And "features/step_definitions/steps.ads" should contain
      """
        --  @then ^tata$
        procedure Then_tata (Args : in out Arg_Type);
      """
    And "features/step_definitions/steps.adb" should exist
    And "features/step_definitions/steps.adb" should contain
      """
      package body Steps is

         procedure Given_a_computer (Args : in out Arg_Type) is
      """
    And "features/step_definitions/steps.adb" should contain
      """
         end Given_a_computer;

         procedure Mixed_Step (Args : in out Arg_Type) is
      """
    And "features/step_definitions/steps.adb" should contain
      """
         end Mixed_Step;

         procedure When_toto (Args : in out Arg_Type) is
      """
    And "features/step_definitions/steps.adb" should contain
      """
         end When_toto;

         procedure Then_tata (Args : in out Arg_Type) is
      """
    And "features/step_definitions/steps.adb" should contain
      """
         end Then_tata;

      --  @xreq insert above

      end Steps;
      """

  @lang @lang-Ada
  Scenario: Filling existing steps
    Given a file "features/step_definitions/steps.ads":
      """
      with XReqLib.General;
      use  XReqLib.General;
      package Steps is

         --  @given ^a computer$
         procedure Given_a_computer (Args : in out Arg_Type);

         --  @when ^I type on my keyboard "([^"]*)"$
         --  @then ^I should see "([^"]*)"$
         --  @todo

      end Steps;
      """
    Given a file "features/step_definitions/steps.adb":
      """
      package body Steps is

         procedure Given_a_computer (Args : in out Arg_Type) is
            Not_Yet_Implemented : exception;
         begin
            raise Not_Yet_Implemented;
         end Given_a_computer;

      end Steps;
      """
    When I run xreq --fill-steps -s features/step_definitions
    Then it should pass
    And "features/step_definitions/steps.ads" should contain
      """
         --  @when ^I type on my keyboard "([^"]*)"$
         --  @then ^I should see "([^"]*)"$
         procedure Mixed_Step (Args : in out Arg_Type);
      """
    And "features/step_definitions/steps.adb" should exist
    And "features/step_definitions/steps.adb" should contain
      """
      package body Steps is

         procedure Given_a_computer (Args : in out Arg_Type) is
            Not_Yet_Implemented : exception;
         begin
            raise Not_Yet_Implemented;
         end Given_a_computer;

         procedure Mixed_Step (Args : in out Arg_Type) is
      """
    And "features/step_definitions/steps.adb" should contain
      """
         end Mixed_Step;

      end Steps;
      """

  @lang @lang-Ada
  Scenario: Filling new steps
    When I run xreq --fill-steps-in new_steps -x suite -m features/test.feature
    Then it should pass
    And  "features/step_definitions/new_steps.ads" should exist
    And  "features/step_definitions/new_steps.adb" should exist
    And  "features/step_definitions/new_steps.ads" should contain
      """
      with XReqLib.General;
      use  XReqLib.General;
      package new_steps is

      """
    And  "features/step_definitions/new_steps.ads" should contain
      """

         --  @given ^a computer$
         procedure Given_a_computer (Args : in out Arg_Type);

      """
    And  "features/step_definitions/new_steps.ads" should contain
      """

         --  @when ^I type on my keyboard "([^"]*)"$
         procedure When_I_type_on_my_keyboard (Args : in out Arg_Type);

      """
    And  "features/step_definitions/new_steps.ads" should contain
      """

         --  @then ^I should see "([^"]*)"$
         procedure Then_I_should_see (Args : in out Arg_Type);

      """
    And  "features/step_definitions/new_steps.ads" should contain
      """

      end new_steps;
      """
    And  "features/step_definitions/new_steps.adb" should contain
      """
      procedure Given_a_computer (Args : in out Arg_Type) is
      """
    And  "features/step_definitions/new_steps.adb" should contain
      """
      procedure When_I_type_on_my_keyboard (Args : in out Arg_Type) is
      """
    And  "features/step_definitions/new_steps.adb" should contain
      """
      procedure Then_I_should_see (Args : in out Arg_Type) is
      """

    And "features/tests/suite" should exist

    When I run the test suite "features/tests/suite"
    Then it should fail with
      """
      Feature: TEST

        Scenario:
          Given a computer
            NEW_STEPS.GIVEN_A_COMPUTER.NOT_YET_IMPLEMENTED: new_steps.adb:12
          When I type on my keyboard "toto"
          Then I should see "toto"

      features/tests/suite features/test.feature:3

      1 scenario (1 failed)
      3 steps (1 failed, 2 skipped)

      """


  @lang @lang-Ada
  Scenario: Filling new steps in an existing step package
    Given a file "features/step_definitions/a_step_package.ads":
      """
      package A_Step_Package is
         --  This is A_Step_Package
      end A_Step_Package;
      """
    Given a file "features/step_definitions/new_steps.ads":
      """
      package New_Steps is
         --  This is my New_Steps package

      end New_Steps;
      """
    Given a file "features/step_definitions/new_steps.adb":
      """
      package body New_Steps is
         --  This is my New_Steps package

      end New_Steps;
      """
    Given a file "features/step_definitions/new_steps2.ads":
      """
      package New_Steps2 is
         --  This is my New_Steps package
         --  @given ^toto$
         --  @todo

      end New_Steps2;
      """

    When I run xreq --fill-steps-in new_steps -x suite -m features/test.feature
    Then it should pass
    And  "features/step_definitions/new_steps.ads" should exist
    And  "features/step_definitions/new_steps.adb" should exist
    And  "features/step_definitions/new_steps.ads" should contain
      """
      with XReqLib.General;
      use  XReqLib.General;
      package New_Steps is
         --  This is my New_Steps package

      """
    And  "features/step_definitions/new_steps.ads" should contain
      """

         --  @given ^a computer$
         procedure Given_a_computer (Args : in out Arg_Type);

      """
    And  "features/step_definitions/new_steps.ads" should contain
      """

         --  @when ^I type on my keyboard "([^"]*)"$
         procedure When_I_type_on_my_keyboard (Args : in out Arg_Type);

      """
    And  "features/step_definitions/new_steps.ads" should contain
      """

         --  @then ^I should see "([^"]*)"$
         procedure Then_I_should_see (Args : in out Arg_Type);

      """
    And  "features/step_definitions/new_steps.ads" should contain
      """

      end New_Steps;
      """

    And  "features/step_definitions/new_steps.adb" should contain
      """
      package body New_Steps is
         --  This is my New_Steps package

      """
    And  "features/step_definitions/new_steps.adb" should contain
      """
      procedure Given_a_computer (Args : in out Arg_Type) is
      """
    And  "features/step_definitions/new_steps.adb" should contain
      """
      procedure When_I_type_on_my_keyboard (Args : in out Arg_Type) is
      """
    And  "features/step_definitions/new_steps.adb" should contain
      """
      procedure Then_I_should_see (Args : in out Arg_Type) is
      """

    And "features/tests/suite" should exist

    When I run the test suite "features/tests/suite"
    Then it should fail with
      """
      Feature: TEST

        Scenario:
          Given a computer
            NEW_STEPS.GIVEN_A_COMPUTER.NOT_YET_IMPLEMENTED: new_steps.adb:13
          When I type on my keyboard "toto"
          Then I should see "toto"

      features/tests/suite features/test.feature:3

      1 scenario (1 failed)
      3 steps (1 failed, 2 skipped)

      """

  @lang @lang-Ada
  Scenario: Filling new steps in an existing step package containing @todo
    Given a file "features/step_definitions/steps.ads":
      """
      package Steps is
         --  This is my New_Steps package
         --  @given ^toto$
         --  @todo

      end Steps;
      """
    Given a file "features/step_definitions/new_steps.ads":
      """
      package New_Steps is
         --  This is my New_Steps package
         --  @given ^tata$
         --  @todo

      end New_Steps;
      """

    When I run xreq --fill-steps-in new_steps -x suite -m features/test.feature
    Then it should pass
    And  "features/step_definitions/steps.ads" should exist
    And  "features/step_definitions/steps.adb" should exist
    And  "features/step_definitions/new_steps.ads" should exist
    And  "features/step_definitions/new_steps.adb" should exist
    And  "features/step_definitions/new_steps.ads" should contain
      """
      with XReqLib.General;
      use  XReqLib.General;
      package New_Steps is

      """
    And  "features/step_definitions/new_steps.ads" should contain
      """

         --  @given ^tata$
         procedure Given_tata (Args : in out Arg_Type);

      """
    And  "features/step_definitions/new_steps.adb" should contain
      """
         procedure Given_tata (Args : in out Arg_Type) is

      """
    And  "features/step_definitions/steps.ads" should contain
      """
      with XReqLib.General;
      use  XReqLib.General;
      package Steps is

      """
    And  "features/step_definitions/steps.ads" should contain
      """

         --  @given ^toto$
         procedure Given_toto (Args : in out Arg_Type);

      """
    And  "features/step_definitions/steps.adb" should contain
      """
         procedure Given_toto (Args : in out Arg_Type) is

      """

    And  "features/step_definitions/new_steps.ads" should contain
      """

         --  @then ^I should see "([^"]*)"$
         procedure Then_I_should_see (Args : in out Arg_Type);

      """
