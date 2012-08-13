Feature: Bootstrapping
  In order to test XReq
  As a developper of XReq
  I want to run XReq on the cucumber test suite

  Background:
    Given xreq is in the PATH

  @bootstrap
  Scenario: Bootstrap
    Given I am in the xreq directory
    When I run "gmake clean" in features/tests
    Then it should pass
    When I run xreq -m -x bootstrap_suite features/*.feature
    Then it should pass
    When I run "features/tests/bootstrap_suite -d -t ~@bootstrap+~@wip -f html -o reports/features-xreq.html"
    Then it should pass
    And "reports/features-xreq.html" should exist
