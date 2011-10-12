Feature: Hooks in XReq
  In order to be able to get metrics on code
  As a developper
  I want to bea ble to run commands at some points in XReq

  Background:
    Given xreq is in the PATH
    And I am in the xreq directory

  Scenario: XREQ_BEFORE_MAKE: Run a command that succeed
    When I run "XREQ_BEFORE_MAKE_SILENT= XREQ_BEFORE_MAKE=true xreq.dbg -m -x suite features/data/simplest.feature"
    Then it should pass
    And the output should contain
      """
      --> Execute XREQ_BEFORE_MAKE: true
      --> Success

      --> Generate Test Suite: suite

      """

  Scenario: XREQ_BEFORE_MAKE: Run a command that fail
    When I run "XREQ_BEFORE_MAKE_SILENT= XREQ_BEFORE_MAKE=false xreq.dbg -m -x suite features/data/simplest.feature"
    Then it should pass
    And the output should contain
      """
      --> Execute XREQ_BEFORE_MAKE: false
      --> Failure: 1

      --> Generate Test Suite: suite

      """

  Scenario: XREQ_BEFORE_MAKE: Run a command silently
    When I run "XREQ_BEFORE_MAKE_SILENT=true XREQ_BEFORE_MAKE=true xreq.dbg -m -x suite features/data/simplest.feature"
    Then it should pass
    And the output should not contain
      """
      --> Execute XREQ_BEFORE_MAKE: true

      """
