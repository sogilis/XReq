Feature: Bootstrapping
  In order to test AdaSpec
  As a developper of AdaSpec
  I want to run Adaspec on the cucumber test suite

  Background:
    Given adaspec is in the PATH

  @bootstrap
  Scenario: Bootstrap
    Given I am in the adaspec directory
    When I run "make clean" in features/tests
    Then it should pass
    When I run adaspec -m -x bootstrap_suite features/*.feature
    Then it should pass
    When I run "features/tests/bootstrap_suite -t ~@bootstrap+~@wip -f html -o reports/features-adaspec.html"
    Then it should pass
    And "reports/features-adaspec.html" should exist
