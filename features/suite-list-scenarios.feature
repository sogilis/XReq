Feature: List all scenarios compiled in a test suite
  In order to run scenarios conditionally based on their feature file name and
  scenario sequence number
  As an test suite runner
  I want to see the list of compiled scenarios in a test suite in the form of

  FEATURE_FILENAME:SCENARIO_NUM

  Background:
    Given xreq is in the PATH
    And I am in the xreq directory

  Scenario: Command line --list
    Given a file "features/data/tmp-a.feature":
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
    Given a file "features/data/tmp-b.feature":
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
    When I run xreq -m -x suite features/data/tmp-a.feature features/data/tmp-b.feature
    Then it should pass

    When I run "./suite --list" in features/data/tests
    Then it should pass with
      """
      Feature: A

        features/data/tmp-a.feature#1:10 S1
        features/data/tmp-a.feature#2:13 S2
        features/data/tmp-a.feature#3:16 S3

      Feature: B

        features/data/tmp-b.feature#1:10 S1
        features/data/tmp-b.feature#2:13 S2
        features/data/tmp-b.feature#3:16 S3

      """
