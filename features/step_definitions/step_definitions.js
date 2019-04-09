const { Given, When, Then, setDefaultTimeout } = require('cucumber');
const assert = require('assert');
const {Builder, By, Key, until} = require('selenium-webdriver');
var chrome = require('selenium-webdriver/chrome');
var chromeOptions = new chrome.Options();
chromeOptions.addArguments("--auto-open-devtools-for-tabs");
let driver = new Builder().forBrowser("chrome")
    .setChromeOptions(chromeOptions)
    .build();

setDefaultTimeout(15 * 1000);

Given('Browse to URL {string}', async function(string) {
	await driver.get(string);
});

Given('Input {string} to {string}', function(string, string2, callback) {
    driver.findElement(By.id(string2))
    .then(
        function(element) {
            element.sendKeys(string);
        },
        function(err) {
            callback(err);
        }
    );
});

When('Click {string}', function(string) {
    // Write code here that turns the phrase above into concrete actions
    return 'pending';
});

Then('Check field {string} receives {string} in {string}', function(string, string2, string3) {
    // Write code here that turns the phrase above into concrete actions
    return 'pending';
});

