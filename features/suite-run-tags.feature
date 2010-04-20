Feature: Tags
  In order to run scenarios conditionally
  As an spec writer
  I want to specify tags to scenarios

  Background:
    Given xreq is in the PATH
    And I am in the xreq directory

  Scenario: Show tags
    Given a file "features/data/tmp-tags.feature":
      """
      Feature: F

        @tagB

        Background:
          Given this step doesn't work

        @tag1 @tag2
        Scenario: S
          Given this step works
      """
    When I run xreq -x suite features/data/tmp-tags.feature
    Then it should pass

    When I compile "suite" in features/data/tests
    Then it should pass

    When I run the test suite "./suite" in features/data/tests
    Then it should fail with
      """
      Feature: F

        @tagB
        Background:
          Given this step doesn't work
            XREQLIB.ASSERTS.ERROR: Error message

        @tag1
        @tag2
        Scenario: S
          Given this step works

      1 scenario (1 failed)
      2 steps (1 failed, 1 skipped)

      """

    When I run the test suite "./suite -f html -o report.html" in features/data/tests
    Then it should fail
    And "features/data/tests/report.html" should exist
    And "features/data/tests/report.html" should contain
      """
      @tagB
      """
    And "features/data/tests/report.html" should contain
      """
      @tag1
      """
    And "features/data/tests/report.html" should contain
      """
      @tag2
      """

  Scenario: Show tags (2)
    Given a file "features/data/tmp-tags.feature":
      """
      Feature: F

        @tagB @tagC

        Background:
          Given this step doesn't work

        @tag1 @tag2
        Scenario: S
          Given this step works
      """
    When I run xreq -x suite features/data/tmp-tags.feature
    Then it should pass

    When I compile "suite" in features/data/tests
    Then it should pass

    When I run the test suite "./suite" in features/data/tests
    Then it should fail with
      """
      Feature: F

        @tagB
        @tagC
        Background:
          Given this step doesn't work
            XREQLIB.ASSERTS.ERROR: Error message

        @tag1
        @tag2
        Scenario: S
          Given this step works

      1 scenario (1 failed)
      2 steps (1 failed, 1 skipped)

      """

    When I run the test suite "./suite -f html -o report.html" in features/data/tests
    Then it should fail
    And "features/data/tests/report.html" should exist
    And "features/data/tests/report.html" should contain
      """
      @tagB
      """
    And "features/data/tests/report.html" should contain
      """
      @tagC
      """
    And "features/data/tests/report.html" should contain
      """
      @tag1
      """
    And "features/data/tests/report.html" should contain
      """
      @tag2
      """

  Scenario: Conditional execution
    Given a file "features/data/tmp-conditional.feature":
      """
      Feature: Feature
        This is executed conditionnally

        @tagB1 @tagB2
        Background:
          Given this step works

        @tag1 @tag1a
        Scenario: S1
          Given this step works

        @tag2 @tag2a
        Scenario: S2
          Given this step works
      """
    When I run xreq -x suite features/data/tmp-conditional.feature
    Then it should pass

    When I compile "suite" in features/data/tests
    Then it should pass

    When I run the test suite "./suite -t @tag1" in features/data/tests
    Then it should pass with
      """
      Feature: Feature
        This is executed conditionnally

        @tagB1
        @tagB2
        Background:
          Given this step works

        @tag1
        @tag1a
        Scenario: S1
          Given this step works

      1 scenario (1 passed)
      2 steps (2 passed)

      """

  Scenario: Negative conditional execution
    Given a file "features/data/tmp-conditional.feature":
      """
      Feature: Feature
        This is executed conditionnally

        @tagB1 @tagB2
        Background:
          Given this step works

        @tag1 @tag1a
        Scenario: S1
          Given this step works

        @tag2 @tag2a
        Scenario: S2
          Given this step works
      """
    When I run xreq -x suite features/data/tmp-conditional.feature
    Then it should pass

    When I compile "suite" in features/data/tests
    Then it should pass

    When I run the test suite "./suite -t ~@tag1" in features/data/tests
    Then it should pass with
      """
      Feature: Feature
        This is executed conditionnally

        @tagB1
        @tagB2
        Background:
          Given this step works

        @tag2
        @tag2a
        Scenario: S2
          Given this step works

      1 scenario (1 passed)
      2 steps (2 passed)

      """

  Scenario: And conditional execution
    Given a file "features/data/tmp-conditional.feature":
      """
      Feature: Feature
        This is executed conditionnally

        @tagB1 @tagB2
        Background:
          Given this step works

        @tag1 @tag1a
        Scenario: S1
          Given this step works

        @tag2 @tag2a
        Scenario: S2
          Given this step works

        @tag1 @t
        Scenario: S3
          Given this step works
      """
    When I run xreq -x suite features/data/tmp-conditional.feature
    Then it should pass

    When I compile "suite" in features/data/tests
    Then it should pass

    When I run the test suite "./suite -t @tag1+@tag2" in features/data/tests
    Then it should pass with
      """
      0 scenarios
      0 steps

      """

    When I run the test suite "./suite -t @tag1+~@tag2" in features/data/tests
    Then it should pass with
      """
      Feature: Feature
        This is executed conditionnally

        @tagB1
        @tagB2
        Background:
          Given this step works

        @tag1
        @tag1a
        Scenario: S1
          Given this step works

        @tag1
        @t
        Scenario: S3
          Given this step works

      2 scenarios (2 passed)
      4 steps (4 passed)

      """


    When I run the test suite "./suite -t ~@t+~@tag2" in features/data/tests
    Then it should pass with
      """
      Feature: Feature
        This is executed conditionnally

        @tagB1
        @tagB2
        Background:
          Given this step works

        @tag1
        @tag1a
        Scenario: S1
          Given this step works

      1 scenario (1 passed)
      2 steps (2 passed)

      """


  Scenario: Or conditional execution
    Given a file "features/data/tmp-conditional.feature":
      """
      Feature: Feature
        This is executed conditionnally

        @tagB1 @tagB2
        Background:
          Given this step works

        @tag1 @tag1a
        Scenario: S1
          Given this step works

        @tag2 @tag2a
        Scenario: S2
          Given this step works

        @tag1 @t
        Scenario: S3
          Given this step works
      """
    When I run xreq -x suite features/data/tmp-conditional.feature
    Then it should pass

    When I compile "suite" in features/data/tests
    Then it should pass

    When I run the test suite "./suite -t @tag1a,@tag2" in features/data/tests
    Then it should pass with
      """
      Feature: Feature
        This is executed conditionnally

        @tagB1
        @tagB2
        Background:
          Given this step works

        @tag1
        @tag1a
        Scenario: S1
          Given this step works

        @tag2
        @tag2a
        Scenario: S2
          Given this step works

      2 scenarios (2 passed)
      4 steps (4 passed)

      """

    When I run the test suite "./suite -t ~@tag1,@t" in features/data/tests
    Then it should pass with
      """
      Feature: Feature
        This is executed conditionnally

        @tagB1
        @tagB2
        Background:
          Given this step works

        @tag2
        @tag2a
        Scenario: S2
          Given this step works

        @tag1
        @t
        Scenario: S3
          Given this step works

      2 scenarios (2 passed)
      4 steps (4 passed)

      """


    When I run the test suite "./suite -t ~@tag1,~@tag1a" in features/data/tests
    Then it should pass with
      """
      Feature: Feature
        This is executed conditionnally

        @tagB1
        @tagB2
        Background:
          Given this step works

        @tag2
        @tag2a
        Scenario: S2
          Given this step works

        @tag1
        @t
        Scenario: S3
          Given this step works

      2 scenarios (2 passed)
      4 steps (4 passed)

      """
