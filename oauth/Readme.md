# Introduction

Trying to learn about OAuth.



# Resources

[1] Overview of OAuth 2: https://www.slideshare.net/zeronine1/auth-in-the-extended-enterprise-mit-hackathon-2013

[2] Discussion of OAuth 2: https://oauth.net/articles/authentication/

[3] OAuth 2 API: https://swagger.io/docs/specification/authentication/oauth2/

[4] Introduction to OAuth 2: https://www.digitalocean.com/community/tutorials/an-introduction-to-oauth-2

[5] OAuth 2 flows: https://oauth.net/2/grant-types/

[6] OAuth 2 authorization code grant type: https://developer.okta.com/blog/2018/04/10/oauth-authorization-code-grant-type

[7] What is OAuth 2: https://developer.okta.com/blog/2017/06/21/what-the-heck-is-oauth

[8] The password anti-pattern: https://arstechnica.com/information-technology/2010/01/oauth-and-oauth-wrap-defeating-the-password-anti-pattern/

[9] Udemy "Oauth 2 Simplified" Course: https://goodleap.udemy.com/course/oauth-2-simplified/learn/lecture/23718056#overview



# OAuth 2.0 Essentials

## Definition - [7], [1] Slide 6

OAuth it’s an open standard for authorization and anyone can implement it.

More specifically, OAuth is a standard that apps can use to provide client applications with “secure delegated access”. OAuth works over HTTPS and authorizes devices, APIs, servers, and applications with access tokens rather than credentials.

OAuth is built on experience with OAuth 1, SAML, OpenID and others.  Built for HTTP APIs; mobile and REST-friendly.  IETF Standard RFC6749, RFC6750.

## Motivation - [7], [8]

OAuth was created as a response to the direct authentication pattern.

This pattern was made famous by HTTP Basic Authentication, where the user is prompted for a username and password, then is allowed access everywhere.  This is often called the password anti-pattern:

A user enters their credentials not only on the target site, but also in other apps (web, mobile, desktop) that provide the user with various services.  Once various apps got the user's credentials, they can do anything on behalf of the user.  There are at least two big issues here:

- It's virtually impossible to revoke access for a certain app without changing your password.
- There is no limit to what the app can do.

The solution that emerged is having a authorization system where the user provides their credentials in a single place, and then allow/revoke access for certain apps to certain resources.  OAuth has appeared as an attempt to unify these authorization systems under a single standard.

## Key Components - [1] Slide 7, [4], [7], [9]

- Actors:
	- **Resource Owner** (**RO**): User that controls stuff
	- **User Agent**: Means by which RO acts, e.g. a browser or a mobile app
	- **Client**: Application that wants stuff
	- **Resource Server** (**RS**): Server that has stuff
	- **Authorization Server** (**AS**): Server that issues tokens
- Tokens:
	- **Access token**: Lets Client get stuff
	- **Refresh token**: Lets Client ask for access tokens w/o bugging the user again
- **Flows**: How is Client getting an access token
- **Scope**: What is Client (not) allowed to do
- **Registration**: Register Client with AS/RS

## Clients, User Consent, and Channels

A Client is an application that wants to use resources controlled by the RO.  It has it's own **identity**, represented by a **Client ID** that identifies the Client throughout the authentication flows.

There are two types of a Client:

- **Confidential client**: Has credentials (typically a client secret) that it can use during the authentication flow:
	- String client secret
	- Public/private key (more secure)
- **Public client**: Does not have credentials

The **user consent** page is utilized to enable AS know for sure the User is actually trying to do the things that the Client claims.

**Channels** are means of communication between the Client and the AS:

- The **front channel** is using a browser redirects via the address bar to transfer data (query parameters), and is not secure.  But it does allow AS to know that the User is there
- The **back channel** is happening behind the sceens between Client and AS, using HTTPS requests, and is secure

## General Use Case - [1] Slides 11+, [7], [9]

```plantuml
actor RO as "Resource Owner\nUser Agent"
collections Client as "Client"
collections Confidential as "Confidential\nClient"
collections Public as "Public\nClient"
entity RS as "Resource Server"
control AS as "Authorization Server"

AS <== RO: Logins
RO => Client: Delegates for Scope
AS <== Client: Obtains access token
Client => RS: Uses token within scope
Client <|-- Confidential
Client <|-- Public
```

## Flows - [1] Slide 8, [4], [5], [6]

In OAuth 2, "flow" is a synonym to "grant type", and refers to the way a client gets an access token.

Common OAuth 2 flows:

- Authorization code: very secure, for server and native apps
- Client credentials: for server-to-server access
- Implicit: not that secure, for mobile and web apps, not recommended
- Resource owner password credentials: legacy, similar to direct authentication, not recommended

## Flow: Implicit - [4]

Not recommended, because there's no way for AS to be sure that the `ACCESSTOKEN` would be actually delivered back to the application in the process of that redirect.  Similarly, the Client cannot be sure that the `ACCESSTOKEN` it got is from the AS.

This flow only exists because some years ago, browsers wouldn't allow cross-origin requests, so JavaScript applications had no way of using the back channel.

```plantuml
skinparam sequenceArrowThickness 2

actor RO as "Resource Owner\nUser Agent"
collections Client as "Client"
entity RS as "Resource Server"
control AS as "Authorization Server"

activate RO
RO -[#Blue]> Client: Do **ACTION**

activate Client #LightSkyBlue
Client -[#Blue]> RO: Redirect to AS\n  Includes Client ID, Redirect URL
RO -> AS: Authenticate

activate AS #DarkSalmon
AS-> RO: Allow Client to access X, Y?
RO -> AS: Allow it
AS -> RO: Redirect to Client, keep **ACCESSTOKEN**
RO -[#Blue]> Client: I authorized you
Client -[#Blue]> RO: Script to extract token
RO -[#Black]> RO: Extract **ACCESSTOKEN**
RO -[#Blue]> Client: **ACCESSTOKEN**
deactivate AS

Client -[#GoldenRod]> RS: Get X w/ **ACCESSTOKEN**

activate RS #Gold
RS -[#GoldenRod]> Client: X
deactivate RS

Client -[#Blue]> RO: **ACTION** done
deactivate Client
```

## Flow: Authorization Code - [1] Slides 11+

In this flow, the back channel is used to obtain the token.  The client secret is used to prove that the client is who it claims to be.

For apps without a client secret, such as mobile, there are to components to secure this:

- The client builds a temporary key (**PCKE**), includes it with its first request, as well as the token request.  This adds more security but still doesn't prevent an attacker from impersonating Client right from the beginning
- The client's **Redirect URL** might serve as a proof of identity, if the URL is HTTPS based, and that's because HTTPS domain are owned by a single entity.  But with mobile, the Redirect URL is not necessarily unique to a single app, although recently this started to change

This is nowhere near as secure as a client secret, but it's much better than nothing.

```plantuml
skinparam sequenceArrowThickness 2

actor RO as "Resource Owner\nUser Agent"
collections Client as "Client"
entity RS as "Resource Server"
control AS as "Authorization Server"

activate RO
RO -[#Blue]> Client: Do **ACTION**

activate Client #LightSkyBlue
Client -[#Blue]> RO: Redirect to AS\n  Includes Client ID, **Redirect URL**, **[PKCE]**
RO -> AS: Authenticate

activate AS #DarkSalmon
AS-> RO: Allow Client to access X, Y?
RO -> AS: Allow it
AS -> RO: Redirect to Client w/ **AUTHCODE**
RO -[#Blue]> Client: **AUTHCODE**
Client -[#Salmon]> AS: **AUTHCODE** & **Client Secret** / **PKCE**
AS -[#Salmon]> Client: **ACCESSTOKEN**
deactivate AS

Client -[#GoldenRod]> RS: Get X w/ **ACCESSTOKEN**

activate RS #Gold
RS -[#GoldenRod]> Client: X
deactivate RS

Client -[#Blue]> RO: **ACTION** done
deactivate Client
```

## Flow: Client Credentials - [4]

The reason the Client doesn't use its Secret to go directly to the RS is multi-fold:

- The RS would need to be able to process Client secrets
- It would need to know about the Clients, and that would become a problem with distributed RS's

```plantuml
skinparam sequenceArrowThickness 2

collections Client as "Client"
entity RS as "Resource Server"
control AS as "Authorization Server"

activate Client #LightSkyBlue
Client -[#Salmon]> AS: Client / App Own Credentials \n(no Resource Owner involved)

activate AS #DarkSalmon
AS -[#Salmon]> Client: **ACCESSTOKEN**
deactivate AS

Client -[#GoldenRod]> RS: Get X w/ **ACCESSTOKEN**

activate RS #Gold
RS -[#GoldenRod]> Client: X
deactivate RS

```



# OpenID Connect & Tokens

**OpenID Connect** (**OIDC**) is an extension to OAuth that provides information about users to applications (Clients).

OIDC adds an **ID token** into the OAuth mix.  AS returns it in order to communicate information about users to the application.

The ID token uses the JWT (JSON Web Token) format:

- Header: Information about the token
- Payload: Actual data
- Signature: Information about the issuer

## ID vs Access Tokens

When it comes to format, acces tokens often also use JWT.  But their purpose is completely different than that of an ID token.

Access token:

- Is used by Client to access resources into RS
- It's meant to be, and should be, opaque to the Client
- Client should make no assumptions on its format

ID token:

- Is used by Client to learn information about the User
- It's meant to be read by the Client
- Client must be able to decode and use the information packed in it

It is the Audience (`aud`) field that differentiates between the two: ID tokens are targeted at the Client, thus may have the Client ID as the value, while access tokens are targeted at the RS, thus using some sort of an identifier of the API it unlocks.

## Obtaining Tokens

### Redirect to AS

Client sends the redirect including all info to identify itself and request the scopes of access:

```Ini
https://authorization-server.com/auth?
	response_type=code
	&scope=photos+openid
	&client_id=CLIENT_ID
	&redirect_uri=https://example-app.com/redirect
	&state=xyz1234
	&nonce=1029343434
	&code_challenge=CODE_CHALLENGE
	&code_challenge_method=S256
```

AS responds with redirecting to the specified URI including the code:

```Ini
https://example-app.com/redirect?
	code=xxxxxxxxxxx
	&state=xyz1234
```

### Get the Tokens

Once the **Client** got the code, it will use it along with its client ID and secret to get both tokens:

```Ini
POST /oauth/token HTTP/1.1
Host: authorization-server.com
 
grant_type=authorization_code
&code=xxxxxxxxxxx
&redirect_uri=https://example-app.com/redirect
&code_verifier=Th7UHJdLswIYQxwSg29DbK1a_d9o41uNMTRmuH0PM8zyoMAQ
&client_id=xxxxxxxxxx
&client_secret=xxxxxxxxxx
```

**AS** responds, including also the ID token:

```JSON
HTTP/1.1 200 OK
Content-Type: application/json
Cache-Control: no-store
 
{
  "access_token":"MTQ0NjJkZmQ5OTM2NDE1ZTZjNGZmZjI3",
  "token_type":"Bearer",
  "expires_in":3600,
  "refresh_token":"IwOGYzYTlmM2YxOTQ5MGE3YmNmMDFkNTVk",
  "scope":"create",
  "id_token":"ewogICJpc3MiOiAiaHR0cDovL..."
}
```

### User Information

If the Client only adds `openid` into the `scope`, then it will only get the user's ID or subject.  There are more things that can be obtained about a user, such as: `profile`, `email`, `address`, `phone`.  These need to be added to the `scope` by the Client when making the initial redirect request.

Not always all of this information will be included directly inside the token.  Sometimes just a resource URI will be provided and the Client will need to call it using some access token.

### ID Token Security

**Authorization Code Flow**

If Client gets the ID token like above, via the Authorization Code flow, this means that Client can skip validating the token, because it got it via the back channel, which is secure and guarantees the identity of the parties involved in the exchange.

**Implicit Flow**

If instead of `response_type=code` above, the Client uses `response_type=id_token`, the AS will return the `id_token` in the redirect instead of the `code`.  This means the ID token came via the front channel (implicit flow), so it's not secure and needs to be validated.

The ID token includes signature and claims that allow the Client to validate it.  However, the AS will be in the dark as of whether the requested information got to the Client.

So in this flow, it's better to disallow obtaining sensitive information, such as email.

### OpenID Hybrid Flows

OIDC defines some combination of flows, that are known as **hybrid flows**.  Thus, the Client can request in it's initial redirect:

- `response_type=code+id_token`, which combines *Authorization Code* for the Access Token with *Implicit* for the ID Token
- `response_type=token+id_token`, which uses *Implicit* for *both* the Access Token *and* the ID Token.  While OIDC does provide tools for checking the authenticity of both tokens, this is not a recommended flow, since access token could leak via the front channel
- There are other flows, but all of them contain `token`, so they're not recommended

With the `code+id_token` flow, the `id_token` will contain a claim called `c_hash`, that has a hash of the `code`, which can be used to validate the latter.

A possible reason to use this flow is when you want your user information ahead of the access token, e.g. for performance.

### Securing the ID Token Implicit Flow

The recommendation is to use PCKE, because in this way both the Client and the AS can make sure the information reaches the desired destination.

### Validating an ID Token

This implies two steps:

- Validate the signature using a JWT library and the AS-provided key, to make sure the token comes from where you expect it to
- Validate some claims inside the token to make sure the token is inteded for your application: `iss`, `aud`, `iat`, `exp`, `nonce`
