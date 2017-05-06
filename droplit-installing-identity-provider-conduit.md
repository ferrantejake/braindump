# Configure Auth0 Identity Service Provider

## Scenario

---

A user needs to authenticate with your service and you allow users to authenticate through some internal or 3rd party service such as Facebook, Auth0, Microsoft, Google, etc. This is typically using the oAuth standard.

## Objective

---

Configure an Identity Provider - We will explore Auth0

## Requirements

---

* Auth0 account
* Droplit account

## Configure Auth0 Client

---

### Get an Auth0 account

1. Go to [https://auth0.com](https://auth0.com) and create an account

### Create an Auth0 client

1. Go to clients \(left side panel\)
2. Click "Create Client" button
3. Choose "Regular Web Application"
4. Add the following callback urls to your new client
   1. [https://yo.droplit.io/oauth2/callback](https://yo.droplit.io/oauth2/callback)
5. Set the token endpoint authentication method to **POST**

### Install Droplit Auth0 conduit

1. In a new tab, go to [http://portal.droplit.io/](http://portal.droplit.io/) and sign in
2. Click "Ecosystems" \(side-left panel\) and select your desired ecosystem
   1. conduits are ecosystem-wide, so we do not need to select an environment
3. Click "Conduits" to open the "Conduits" view
4. Click the "Auth0" conduit and open the Auth0 conduit options
5. Click "Install"
6. Label your conduit aptly - you have one for this ecosystem :\)

### Configure Auth0 client with Droplit Auth0 conduit

1. In the Droplit portal, click on your newly installed Droplit Auth0 conduit
   1. You should see three input fields: Client Id, Client Secret, Domain
2. Go back to your Auth0 tab or open a new tab and navigate to [https://auth0.com](https://auth0.com)
3. Open your newly installed Auth0 client and go to the Settings tab
4. Copy/paste the Client ID, Client Secret, and Domain information from the Auth0 website in the Droplit Auth0 conduit portal configuration
5. Save your Droplit Auth0 conduit configuration

## Conclusion

---

Congratulations, you've completed setting up an Auth0 identity service provider. You can now authenticate with the Auth0 service.

