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




# Lecture Notes

## OAuth 2.0 Concepts

### Definition - [7], [1] Slide 6

OAuth it’s an open standard for authorization and anyone can implement it.

More specifically, OAuth is a standard that apps can use to provide client applications with “secure delegated access”. OAuth works over HTTPS and authorizes devices, APIs, servers, and applications with access tokens rather than credentials.

OAuth is built on experience with OAuth 1, SAML, OpenID and others.  Built for HTTP APIs; mobile and REST-friendly.  IETF Standard RFC6749, RFC6750.

### Motivation - [7], [8]

OAuth was created as a response to the direct authentication pattern.

This pattern was made famous by HTTP Basic Authentication, where the user is prompted for a username and password, then is allowed access everywhere.  This is often called the password anti-pattern:

A user enters their credentials not only on the target site, but also in other apps (web, mobile, desktop) that provide the user with various services.  Once various apps got the user's credentials, they can do anything on behalf of the user.  There are at least two big issues here:

- It's virtually impossible to revoke access for a certain app without changing your password.
- There is no limit to what the app can do.

The solution that emerged is having a authorization system where the user provides their credentials in a single place, and then allow/revoke access for certain apps to certain resources.  OAuth has appeared as an attempt to unify these authorization systems under a single standard.

### Key Components - [1] Slide 7, [4], [7]

- Actors:
	- Resource Owner (RO): User that controls stuff
	- User Agent: Means by which RO acts, usually a browser
	- Client: App that wants stuff
		- Confidential client: Can be trusted to store a secret, running in a safe environment
		- Public client: Running on a desktop or distributed via app store
	- Resource Server (RS): Server that has stuff
	- Authorization Server (AS): Server that issues tokens
- Tokens:
	- Access token: Lets client get stuff
	- Refresh token: Lets client ask for access tokens w/o bugging the user again
- Flows: How is the client getting an access token
- Scope: What is the client (not) allowed to do
- Registration: Register the Client with AS/RS

### General Use Case - [1] Slides 11+, [7]

```plantuml
actor RO as "Resource Owner"
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

### Flows - [1] Slide 8, [4], [5], [6]

In OAuth 2, "flow" is a synonym to "grant type", and refers to the way a client gets an access token.

Common OAuth 2 flows:

- Authorization code: very secure, for server and native apps
- Implicit: for mobile and web apps
- Client credentials: for server-to-server access
- Resource owner password credentials: legacy, similar to direct authentication

### Flow: Authorization Code - [1] Slides 11+

```plantuml
skinparam sequenceArrowThickness 2

actor RO as "Resource Owner\nUser Agent"
collections Client as "Client"
entity RS as "Resource Server"
control AS as "Authorization Server"

activate RO
RO -[#Blue]> Client: Do **ACTION**

activate Client #LightSkyBlue
Client -[#Blue]> RO: Go to AS
RO -> AS: Authenticate

activate AS #DarkSalmon
AS-> RO: Allow Client to access X, Y?
RO -> AS: Allow it
AS -> RO: Go to Client w/ **AUTHCODE**
RO -[#Blue]> Client: **AUTHCODE**
Client -[#Salmon]> AS: **AUTHCODE**
AS -[#Salmon]> Client: **ACCESSTOKEN**
deactivate AS

Client -[#GoldenRod]> RS: Get X w/ **ACCESSTOKEN**

activate RS #Gold
RS -[#GoldenRod]> Client: X
deactivate RS

Client -[#Blue]> RO: **ACTION** done
deactivate Client
```

### Flow: Implicit - [4]

```plantuml
skinparam sequenceArrowThickness 2

actor RO as "Resource Owner\nUser Agent"
collections Client as "Client"
entity RS as "Resource Server"
control AS as "Authorization Server"

activate RO
RO -[#Blue]> Client: Do **ACTION**

activate Client #LightSkyBlue
Client -[#Blue]> RO: Go to AS
RO -> AS: Authenticate

activate AS #DarkSalmon
AS-> RO: Allow Client to access X, Y?
RO -> AS: Allow it
AS -> RO: Go to Client, keep **ACCESSTOKEN**
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

### Flow: Client Credentials - [4]

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

