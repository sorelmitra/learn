import { reject } from "q";

async function addMessage(text) {
  await element(by.id('messageText')).tap();
  await element(by.id('messageText')).typeText(text);
  await element(by.id('messageText')).typeText('\n');
  await expect(element(by.id('messageText'))).toHaveText('');
  await expect(element(by.label(text))).toBeVisible();
}

describe('Conversation', () => {
  beforeEach(async () => {
    await device.reloadReactNative();
  });

  it('should add message to the list', async () => {
    let text = 'Hi There';
    await addMessage(text);
  });

  it('should scroll to the bottom when adding new message', async () => {
    var n = 8;
    for (let i = 0; i < n; i++) {
      let text = 'Message ' + (i+1).toString();
      await addMessage(text);
    }
  });

  it.only('should show post sent', async () => {
    let text = 'Post Me';
    await addMessage(text);
    await expect(element(by.id('messageStatus'))).toBeVisible();
    await expect(element(by.id('messageStatus'))).toHaveText('(sent)');
  });

});
