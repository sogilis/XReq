Feature: Tags
  In order to document the features
  As an spec writer
  I want to be able to write comments

  Background:
    Given xreq is in the PATH
    And I am in the xreq directory

  Scenario:
    Given a file "features/data/tmp-comments.feature":
      """
      Feature: F

        # Comment

        Background:
          Given this step works


        # Comment
        @tag
        # other comment

        Scenario: S
          # Comment
          Given this step works
      """
    When I run xreq features/data/tmp-comments.feature
    Then it should pass