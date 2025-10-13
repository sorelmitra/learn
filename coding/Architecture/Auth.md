# Authentication and Authorization (AuthN/Z)

I believe it is always cheaper to use an existing solution such as Auth0 rather than implement your own.  That being said:

- You might profit from writing a proxy to your chosen solution.  This will allow you to change that solution later, in a transparent way.
- It is worth to understand the concepts, so you know how to relate to them during development.  The OAuth framework is a good read.
- The front-end has a big role to play in AuthN/Z, because access tokens, cookies, and other stuff end up in the browser, where everyone can see them.

More reads:

- [Intelligent Tracking Prevention (ITP)](https://infotrust.com/articles/what-is-itp-cookies/)
	* [More on ITP](https://www.mediaexperts.com/intelligent-tracking-prevention-itp-explained/)
- [Website Conversions](https://www.hotjar.com/blog/website-conversion)
- [SPA and Refresh Token Rotation](https://auth0.com/blog/securing-single-page-applications-with-refresh-token-rotation/)

---
---
---
---
---
---
---
---
---
---
---
---

