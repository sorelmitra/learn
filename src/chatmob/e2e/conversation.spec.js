import { reject } from "q";

let lastAddedText = '';
let maxMessages = 10;
let index = 1;

function getTestIdNr(max, N) {
  return max - (2 * N - 1);
}

async function addMessage(text) {
  await element(by.id('messageText')).typeText(text);
  await element(by.id('messageText')).typeText('\n');
  await expect(element(by.id('messageText'))).toHaveText('');
  await expect(element(by.id(`message-${index}`))).toBeVisible();
  await expect(element(by.id(`message-${index}`))).toHaveText(lastAddedText);
}

describe('Conversation', () => {
  beforeEach(async () => {
  });

  it('should add message to the list', async () => {
    lastAddedText = 'Hi There';
    await element(by.id('messageText')).tap();
    await addMessage(lastAddedText);
  });

  it('should scroll to the bottom when adding new message', async () => {
    await element(by.id('messageText')).tap();
    let manyTextsPrefix = 'Message ';
    for (let i = 0; i < maxMessages; i++) {
      lastAddedText = manyTextsPrefix + (i+1).toString();
      await addMessage(lastAddedText);
    }
  });

  it('should keep scroll position when dismissing keyboard', async () => {
    await element(by.id('conversationList')).scroll(170, 'up');
    let max = 2 * maxMessages;
    id1 = `message-${getTestIdNr(max, 5)}`;
    id2 = `message-${getTestIdNr(max, 7)}`;
    id3 = `message-${getTestIdNr(max, 8)}`;
    await expect(element(by.id(id1))).toBeVisible();
    await expect(element(by.id(id2))).toBeVisible();
    await expect(element(by.id(id3))).toBeNotVisible();
    await element(by.id('conversationList')).tap();
    await expect(element(by.id(id2))).toBeVisible();
    await expect(element(by.id(id3))).toBeNotVisible();
  });
  
  it('should keep scroll position when showing keyboard', async () => {
    await element(by.id('messageText')).tap();
    let max = 2 * maxMessages;
    id1 = `message-${getTestIdNr(max, 5)}`;
    id2 = `message-${getTestIdNr(max, 7)}`;
    id3 = `message-${getTestIdNr(max, 8)}`;
    await expect(element(by.id(id1))).toBeVisible();
    await expect(element(by.id(id2))).toBeVisible();
    await expect(element(by.id(id3))).toBeNotVisible();
  });

  it('should show post sent', async () => {
    await device.reloadReactNative();
    lastAddedText = 'Post Me';
    await element(by.id('messageText')).tap();
    await addMessage(lastAddedText);
    await expect(element(by.id('messageStatus-1'))).toBeVisible();
    await expect(element(by.id('messageStatus-1'))).toHaveText('(sent)');
  });

});
