import { reject } from "q";

async function addMessage(text) {
  await element(by.id('messageText')).typeText(text);
  await element(by.id('messageText')).typeText('\n');
  await expect(element(by.id('messageText'))).toHaveText('');
  await expect(element(by.label(text))).toBeVisible();
}

var lastText = '';

let lastAddedText = '';
let manyTextsPrefix = 'Message ';
let text1 = '';
let text2 = '';
let text3 = '';

describe('Conversation', () => {
  beforeEach(async () => {
    //await device.reloadReactNative();
  });

  it('should add message to the list', async () => {
    lastAddedText = 'Hi There';
    await element(by.id('messageText')).tap();
    await addMessage(lastAddedText);
  });

  it('should scroll to the bottom when adding new message', async () => {
    await element(by.id('messageText')).tap();
    let n = 16;
    for (let i = 0; i < n; i++) {
      lastAddedText = manyTextsPrefix + (i+1).toString();
      await addMessage(lastAddedText);
    }
  });

  it('should keep scroll position when dismissing keyboard', async () => {
    await element(by.id('conversationList')).scroll(2 * 30, 'up');
    text1 = manyTextsPrefix + '10';
    text2 = manyTextsPrefix + '14';
    text3 = manyTextsPrefix + '15';
    await expect(element(by.label(text1))).toBeVisible();
    await expect(element(by.label(text2))).toBeVisible();
    await expect(element(by.label(text3))).toBeNotVisible();
    await element(by.id('conversationList')).tap();
    await expect(element(by.label(text2))).toBeVisible();
    await expect(element(by.label(text3))).toBeNotVisible();
  });
  
  it('should keep scroll position when showing keyboard', async () => {
    await element(by.id('messageText')).tap();
    await expect(element(by.label(text1))).toBeVisible();
    await expect(element(by.label(text2))).toBeVisible();
    await expect(element(by.label(text3))).toBeNotVisible();
  });

  it.skip('should show post sent', async () => {
    lastAddedText = 'Post Me';
    await addMessage(lastAddedText);
    await expect(element(by.id('messageStatus'))).toBeVisible();
    await expect(element(by.id('messageStatus'))).toHaveText('(sent)');
  });

});
