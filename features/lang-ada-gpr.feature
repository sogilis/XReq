Feature: Generate GPR Project file
  In order to compile test suites
  As a test suite user
  I want a generated GPR project file that would
    - include the step definitions used
    - include the generated Ada features
    - compile everything correctly

  Background:
    Given xreq is in the PATH
    And I am in the xreq directory

  @lang @lang-Ada
  Scenario: Option ada.gpr.with with one required project
    When I run xreq -cada.gpr.with=a.gpr -x suite features/data/simplest.feature
    Then it should pass
    And "features/data/tests/suite.gpr" should contain
      """
      with "a.gpr";

      """

  @lang @lang-Ada
  Scenario: Option ada.gpr.with with two required projects
    When I run xreq -cada.gpr.with=a.gpr -cada.gpr.with=b,c.gpr -x suite features/data/simplest.feature
    Then it should pass
    And "features/data/tests/suite.gpr" should contain
      """
      with "b";
      with "c.gpr";

      """