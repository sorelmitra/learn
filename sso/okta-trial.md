# **Okta Application Setup Guide – BlueJayBird**

## **Overview**

This document outlines the steps required to configure an **Okta OIDC (OpenID Connect)** application named **BlueJayBird**.
Follow each section in order to ensure proper setup and integration.

---

## **1. Access the Okta Admin Console**

* Log in to your Okta Admin Dashboard.
* Navigate to:
  **Applications → Applications**
* Select your app: **BlueJayBird**

---

## **2. General Tab Configuration**

### **Client Credentials**

* Copy and securely store the **Client ID** — you’ll need it later.
* Under **Client authentication**, select **Client secret**.
* Leave **Proof Key for Code Exchange (PKCE)** unchecked (unless specifically required).

### **Client Secret**

* If no secret exists, click **Generate new secret**.
* Copy and save the new **Client Secret** securely.
* Ensure its **Status** is set to **Active**.

### **General Settings**

* **App integration name:** BlueJayBird
* **Application type:** Web

**Grant Types** (check all the following):

* ✅ Client Credentials
* ✅ Authorization Code
* ✅ Interaction Code
* ✅ Refresh Token
* ✅ Implicit (hybrid)
* ✅ Allow ID Token with implicit grant type
* ✅ Allow Access Token with implicit grant type

### **Refresh Token Behavior**

* Select **Use persistent token**.

### **User Consent**

* Enable **Require consent**.

---

## **3. Login Configuration**

### **URIs**

* **Sign-in redirect URIs:**
  `https://dev-vx3g0onl.us.auth0.com/login/callback`
* **Sign-out redirect URIs:**
  `http://localhost:8080`

### **Login Behavior**

* **Login initiated by:** Either *Okta* or *App*
* **Application visibility:**

  * ✅ Display application icon to users
  * ✅ Display application icon in Okta Mobile app
* **Login flow:**

  * Select **Redirect to app to initiate login (OIDC Compliant)**
* **Initiate login URI:**
  `http://localhost:8080/login/initiate`

### **Branding (Optional)**

* Add a **Logo URI** for your app if desired.

---

## **4. Email Verification & Federation**

### **Email Verification**

* **Callback URI:** Leave blank unless required by your flow.

### **Federation Broker Mode (Optional)**

* **Immediate app access with Federation:** *Disabled* (default).

---

## **5. App Embed Link**

* Copy the **Embed Link URL** provided under *App Embed Link*.
  Example:
  `https://trial-<your-org>.okta.com/home/oidc_client/...`
* This link allows users to sign in externally via Okta.

---

## **6. Sign On Tab Configuration**

### **Sign-On Method**

* Choose **OpenID Connect**.
* Optional: Configure **profile mapping** if your integration requires custom mappings.

### **Token Credentials**

* **Signing credential rotation:** Automatic.

### **OpenID Connect ID Token**

* **Issuer:** Dynamic (based on request domain)
* **Audience:** App’s ID (e.g., `0oa2qgof1yIzS...`)
* **Claims:** Include all user attributes.
* **Groups claim type:** Filter
* **Groups claim filter:** None *(default, using Groups Claim)*

### **User Authentication Policy**

* **Authentication policy:** Any two factors (default)
* Edit this if your organization requires different authentication rules.

---

## **7. Assignments**

* Open the **Assignments** tab.
* Click **Assign → People** to add individual users.
* Verify that users appear in the list as **Type: Individual**.
* Optionally assign users via **Groups**.

**Reports (for verification):**

* View under:

  * *Current Assignments*
  * *Recent Unassignments*

---

✅ **Setup Complete**

Your Okta application **BlueJayBird** is now properly configured with:

* Client credentials and secret
* Full grant type permissions
* Proper login and redirect URIs
* Correct token and authentication policies
* Assigned users and visibility
