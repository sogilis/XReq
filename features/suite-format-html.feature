Feature: HTML reports
  In order to have good looking reports
  As a person who checks reports
  I want to be able to have HTML reports

  Background:
    Given xreq is in the PATH
    And I am in the xreq directory
    And a file "features/data/tmp-simplest.feature":
      """
      Feature: Sample

      Background: Set things up
        Given this step works
          \"\"\"
          abc
          def
          \"\"\"

      Scenario: Run a good step
        Given this step works

      Scenario: Failure
        Given this step works
        When it fails
        Then do nothing

      """
    And a file "features/data/tmp-simplest2.feature":
      """
      Feature: Periodic Fail

      Background:
        Given this step works
          \"\"\"
          abc
          def
          \"\"\"
        And this fails periodically
        And this is ignored

      Scenario: Run a good step
        Given this step works
        And this is ignored

      Scenario: Run a good step
        Given this step works
        And this is ignored

      Scenario: Run a good step
        Given this step works
        And this is ignored

      Scenario: Run a good step
        Given this step works
        And this is ignored

      """
    And a file "features/data/tmp-fail.feature":
      """
      Feature: Feature that fail

        Background: definitions
          Given it fails
            \"\"\"
            This is a long string
            that take two lines

            \"\"\"
          And this is ignored

        Scenario: A
          Given this step works
          And this is ignored
            \"\"\"
            Another multi-line string
            \"\"\"
          Then do nothing

        Scenario: B
          Given this step works
          And this is ignored
          Then do nothing
            \"\"\"
            Another multi-line string
            \"\"\"
      """
    And a file "features/data/tmp-pass.feature":
      """
      Feature: Feature that pass

        Background: definitions
          Given this step works
          And   I match "this" and "that"
            \"\"\"
            This is a long string
            that take two lines

            \"\"\"
          And this is ignored

        @tagA
        Scenario: A
          Given this step works
          And this is ignored
            \"\"\"
            Another multi-line string
            \"\"\"
          Then do nothing

        @tagB
        Scenario: B
          Given this step works
            | # | My  | Pretty | Table   |
            | 1 | abc | def    | ghi jkl |
            | 2 | fgh | Tggb   | ljIGB J |
            | 3 | d   | khY    | Toto    |
            | 4 | 6ty | JKHG   |
          And this is ignored
          Then do nothing
            \"\"\"
            Another multi-line string
            \"\"\"
      """
    And a file "features/data/tmp-pass2.feature":
      """
      Feature: Feature that pass (2)
        This is the feature description.
        This is another line of description.

        And another paragraph

        Background: definitions
          Given this step works
          And this is ignored
            \"\"\"
            Sample
            \"\"\"
            \"\"\"
            <strong>
            Second
            &&
            String
            </strong>
            \"\"\"

        Scenario:
          Given this step works
            \"\"\"
            Another "multi-line" string
            \"\"\"
          And this is ignored
          Then do nothing
      """

  Scenario: Generate HTML report that fail
    When I run xreq -m -x test_suite_fail features/data/tmp-simplest.feature features/data/tmp-simplest2.feature features/data/tmp-fail.feature
    Then it should pass
    Given I am in "features/data/tests"
    Then "test_suite_fail" should exist
    When I run "./test_suite_fail -d -f html -o report-fail.html"
    Then it should fail
    And  "report-fail.html" should exist
    And  "report-fail.html" should contain
      """
      <html
      """
    When I run "cp report-fail.html ../../../reports/sample-html-fail.html"

  Scenario: Generate HTML report that pass
    When I run xreq -m -x test_suite_pass features/data/tmp-pass.feature features/data/tmp-pass2.feature
    Then it should pass
    Given I am in "features/data/tests"
    Then "test_suite_pass" should exist
    When I run "./test_suite_pass -d -f html -o report-pass.html"
    Then it should pass
    And  "report-pass.html" should exist
    And  "report-pass.html" should contain
      """
      >Sample</pre>
      """
    When I run "cp report-pass.html ../../../reports/sample-html-pass.html"
