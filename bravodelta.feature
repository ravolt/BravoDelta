Feature: Web Test
  In order to create BDD tests
  Developers
  want to use Cucumber like tests for Bravo Delta
  
  Scenario: Web Page1
    When I go to the home page.
    Then I should see "Districts".
    
  Scenario: Web Page Fail
    When I try to go to the "/index.html" page.
    Then I should see "Action Controller: Exception caught".
    Then I should not see "Action Controller: Exception caught".

  Scenario: Web Page2
    When I go to the "/map/bus/xref_10" page.
    Then I should see "bus 10".
    
  Scenario: Link Test
    Given I am on the home page.
    When I click on the "bus 5: active" link.
    Then I should see "bus 5".
    
  Scenario: non WebRat
    When I have a test that is not in WebRat.
    Then I should use my special step file.