const { Given, When, Then, setDefaultTimeout } = require('cucumber');
const assert = require('assert');
const {Builder, By, Key, until} = require('selenium-webdriver');
var chrome = require('selenium-webdriver/chrome');
var chromeOptions = new chrome.Options();
chromeOptions.addArguments("--auto-open-devtools-for-tabs");
let driver = new Builder().forBrowser("chrome")
    .setChromeOptions(chromeOptions)
    .build();

setDefaultTimeout(60 * 1000);

Given('Browse to URL {string}', async function(string) {
	await driver.get(string);
});

Given('Check {string} element contains {string}', async function(string, string2) {
    var element = await driver.findElement(By.xpath(string));
    var value = await element.getText();
    var pattern = ".*" + string2 + ".*";
    var re = new RegExp(pattern, 'i');
    var m = value.match(re);
    console.log("Regexp <%s> found <%s> on <%s>", re, m, value);
    assert(m != null);
});
