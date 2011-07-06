Feature: Calculator

  Background:
    Given the calculator is initialized

  Scenario: addition
    Given the first operand: 1
      And the second operand: 2
     When the two operands are added
     Then the result should be: 3

  Scenario: failed addition
    Given the first operand: 2
      And the second operand: 2
     When the two operands are added
     Then the result should be: 3

