## Overview

The app from `blue-jay-bird` logs in via Auth0 -> Okta Trial, and then can access the user profile information, which is behind the login wall.

## Okta Configuration

The Okta part of the integration was done in Okta Trial -> Applications -> Blue Jay Bird, screenshots in the `okta-trial` folder.  Key items:

- This is an OIDC app.
- The `Sign-in redirect URIs` points to the Auth0's login callback endpoint, which I discovered by using `Try Connection` in the latter.
- In order to have the app appear to its assigned users' dashboard, you need to set `Login initiated` to `Either Okta or App` in the General section of the app.
- The `Initiate login URI` is meant to be used when login is initiated by Okta.
- I had mysteriously obtained a token from our company's directory from the cloud in my `blue-jay-bird` app **when opening the app directly from the Okta dashboard**, although I hadn't set up any connection between the two.  While for Auth0 I discovered the cause (see below), for Okta I'm still not sure where that comes from.

### Auth0 Configuration

The Auth0 part of the integration was done in
Auth0 -> (our test tenant) -> 
	-> Authentication -> Social -> `okta-trial-5286963`
	-> Applications -> `BlueJayBird Okta Trial`

Screenshots in the `auth0-okta` folder.

Key points for the Social connection:

- I have followed the procedure in [here](https://auth0.com/docs/authenticate/identity-providers/social-identity-providers/configure-okta-as-oauth2-identity-provider) with good success.
- The `Scope` must be set to `openid`, because that's what the Fetch User Profile Script uses, and that field doesn't support a list.
- In the `Fetch User Profile Script`, I changed the URL to `https://trial-5286963.okta.com/oauth2/default/v1/userinfo`.
- You have to enable the Application you create below, in the Social Connection Application tab.
- To debug the Fetch User Profile Script, I installed the Extension `Real-time Webtask Logs` in Auth0, and then opened it from the Extensions page.  It shows all `console.log` that you add in the script.

Key points for the Application:

- I left all the fields default except for the following:
	
		Allowed Callback URLs: http://localhost:8080/callback

		Allowed Logout URLs: http://localhost:8080

- I had mysteriously obtained a token from our company's directory in my `blue-jay-bird` app.  I only found the explanation for this when doing the screenshots, when I discovered in the app's `Connections` tab that some of our company's Cognito user pool connections were active.  They probably were enabled by default in all connections for that tenant?  Anyway I disabled those and I no longer get the access token in the cookie.  I only get the session, which (I believe) is much safer.

- I used a NodeJS + Express app, and I followed Auth0's Quick Start tutorial completely.

- In the app, in `sso/blue-jay-bird/src/index.js`, the value `config.secret` ws generated with `openssl rand -hex 32`, as instructed by that Quick Start tutorial.  I'm not sure how this works, could this be a PKCE?

- I saved the Fetch User Profile Script in `blue-jay-bird/src/auth0/scripts` for reference. 

Testing everything:

- Start `blue-jay-bird` locally.
- Point your browser (preferably a dedicated one) to `http://localhost:8080/login`.  This endpoint is added by `express-openid-connect`.
- In the Okta page that appears, choose `Continue with Okta-trial-5286963`.
- You will be redirected to the home page, which displays whether you're logged in or not.
- To logout, open `http://localhost:8080/logout` in the browser.
- To see the user's profile information, open `http://localhost:8080/profile`.  If you're logged in, you'll see the profile information as returned by Okta.  (NOTE: For some reason, the name and nickname are empty, although I've set them on my user in Okta Trial.  But the user ID matches.)  If you're not logged in, you'll be redirected to the login page.


