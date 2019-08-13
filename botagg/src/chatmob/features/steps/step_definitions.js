const { Given, When, Then, setDefaultTimeout } = require('cucumber');
const {expect} = require('chai');

const TIMEOUT = 5000

function waitFor(selector) {
  browser.waitForExist(selector, TIMEOUT)
  const isVisible = browser.isVisible(selector)
  let isFlattedVisible = Array.isArray(isVisible) ? !isVisible.find(visibility => !visibility) : isVisible
  expect(true).to.equal(isFlattedVisible)
}

Given(/^Is visible "([^"]*)?"$/, elementId => {
  waitFor('~' + elementId);
})

When(/^Tap "([^"]*)?"$/, elementId => {
  browser.click('~' + elementId);
})

When(/^Type "([^"]*)?" into "([^"]*)?"$/, (text, elementId) => {
  var element = browser.element('~' + elementId);
  element.addValue(text);
})

When(/^Type ENTER into "([^"]*)?"$/, elementId => {
  var element = browser.element('~' + elementId);
  element.setValue('\n');
})

Then(/^There is text "([^"]*)?" in "([^"]*)?"$/, (text, elementId) => {
  var value = browser.getValue('~' + elementId);
  let flatValue = Array.isArray(value) ? value.find(item => item != null) : value;
  expect(text).to.equal(flatValue);
})

Then(/^Is there an element with id "([^"]*)?"$/, elementId => {
  var element = browser.element('~' + elementId);
  var keys = Object.keys(element);
  console.log("ELEMENT keys: %s", keys);
  for (var key in element) {
    console.log("ELEMENT %s: %s", key, element[key]);
  }
  expect('NoSuchElement').to.not.equal(element.type);
})

