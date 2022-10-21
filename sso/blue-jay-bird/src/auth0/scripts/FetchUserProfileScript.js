const x = function (accessToken, ctx, cb) {
  const url = "https://trial-5286963.okta.com/oauth2/default/v1/userinfo";
  console.log(`Requesting ${url} with access token ${accessToken}`);
  request({
      url: url,
      method: "GET",
      headers: {
        "Authorization": "Bearer " + accessToken,
        "Content-Type": "application/json"
      }
    },
    function (e, r, b) {
      console.log(`Response e=<<<`, e, `>>>`);
      console.log(`Response b=<<<`, b, `>>>`);
      console.log(`Response r=<<<`, r, `>>>`);
      if (e) return cb(e);
      if (r.statusCode !== 200)
        return cb(new Error('StatusCode: ' + r.statusCode + '; ' + r.message));
      const profile = JSON.parse(b);
      profile.user_id = profile.sub;
      delete profile.sub;
      cb(null, profile);
    });
}
