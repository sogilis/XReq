Feature: The simplest works
  In order to use XReq
  As an AsaSpec user
  I want to be able to generate the ads and adb files for a test and the test
  suite

  Background:
    Given xreq is in the PATH
    And I am in the xreq directory
    And a file "features/data/tmp-simplest.feature":
      """
      Feature: Sample
      Description Line 1
      Description Line 2
      Description Line 3

      Background:
        Given this step works loudly

      Scenario: Run a good step
        Given this step works loudly

      """

  @lang @lang-Ada
  Scenario: Test the generation of test packages
    When I run xreq --progress features/data/tmp-simplest.feature
    Then it should pass with
      """
      --> Compile: features/data/tmp-simplest.feature

      Load Ada steps in: features/data/step_definitions
      Compile: features/data/tmp-simplest.feature
      Generate: features/data/tests/feature_tmp_simplest.ads
      Generate: features/data/tests/feature_tmp_simplest.adb
      completed 1 out of 1


      """
    And  "features/data/tests/feature_tmp_simplest.ads" should exist
    And  "features/data/tests/feature_tmp_simplest.adb" should exist
    When I compile "feature_tmp_simplest" in features/data/tests
    Then it should pass


  @lang @lang-Ada
  Scenario: Test the generation of the test suite
    When  I run xreq -m -x simplest_test features/data/tmp-simplest.feature
    Then  it should pass
    And   "features/data/tests/feature_tmp_simplest.ads" should exist
    And   "features/data/tests/feature_tmp_simplest.adb" should exist
    And   "features/data/tests/simplest_test.adb" should exist
    And   "features/data/tests/simplest_test.gpr" should exist
    And   "features/data/tests/simplest_test" should exist
    When  I run the test suite "./simplest_test" in features/data/tests
    Then  it should pass with
      """
      Feature: Sample
        Description Line 1
        Description Line 2
        Description Line 3

        Background:
      This step works
          Given this step works loudly

        Scenario: Run a good step
      This step works
          Given this step works loudly

      1 scenario (1 passed)
      2 steps (2 passed)

      """

  @lang @lang-Ada
  Scenario: Test the generation of the test suite with a project file
    When  I run xreq -x suite features/data/tmp-simplest.feature
    Then  it should pass
    And   "features/data/tests/feature_tmp_simplest.ads" should exist
    And   "features/data/tests/feature_tmp_simplest.adb" should exist
    And   "features/data/tests/suite.adb" should exist
    And   "features/data/tests/suite.gpr" should exist
    When  I run "gnatmake -Psuite.gpr $GNAT_FLAGS" in features/data/tests
    Then  it should pass
    And   "features/data/tests/suite" should exist
    When  I run the test suite "./suite" in features/data/tests
    Then  it should pass with
      """
      Feature: Sample
        Description Line 1
        Description Line 2
        Description Line 3

        Background:
      This step works
          Given this step works loudly

        Scenario: Run a good step
      This step works
          Given this step works loudly

      1 scenario (1 passed)
      2 steps (2 passed)

      """

  Scenario: Test an empty feature
    When I run xreq -m -x empty_suite features/data/empty.feature
    Then it should pass
    When I run the test suite "./empty_suite" in features/data/tests
    Then it should pass with
      """
      Feature: Empty

        Scenario: Empty

      1 scenario (1 passed)
      0 steps

      """

