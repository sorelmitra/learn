const { Given, When, Then, setDefaultTimeout } = require('cucumber');
const chai = require('chai');

Given(/^Tap "([^"]*)?"$/, elementId => {
  browser.click('~' + elementId)
})
