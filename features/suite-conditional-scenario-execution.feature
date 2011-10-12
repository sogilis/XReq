Feature: Run conditionally scenarios
  In order to only run the scenarios I am interested in
  As a test suite runner
  I want to be able to specify on the command line the list of scenarios to
  execute in the form FILENAME:NUM


  Background:
    Given xreq is in the PATH
    And I am in the xreq directory

  Scenario: Conditional run
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

    When I run the test suite "./suite features/data/tmp-a.feature#1 features/data/tmp-b.feature#3" in features/data/tests
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

