const {reloadApp} = require('detox-expo-helpers');

describe('Conversation', () => {
  beforeEach(async () => {
    await reloadApp();
  });

  it('should add message to the list', async () => {
    let text = 'Hi There';
    await element(by.id('messageText')).tap();
    await element(by.id('messageText')).typeText(text);
    await element(by.id('messageText')).typeText('\n');
    await expect(element(by.id('messageText'))).toHaveText('');
    await expect(element(by.label(text))).toBeVisible();
  });

});
