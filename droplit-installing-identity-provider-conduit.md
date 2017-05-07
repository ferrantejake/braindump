# Install Droplit Auth0 Skill

# Overview

This walkthrough shows you how to install the Dropliot Auth0 skill, set up a compatible Auth0 client, and configure the two to work together to provide authentication for your ecosystem. It consists of 3 parts:

* Create/Configure Auth0 Client
* Install Droplit Auth0 Skill
* Configure Droplit Auth0 skill to communicate with Auth0 client 

Once this is completed, users in your ecosystem will be able to authenticate through Atuh0 using one of Auth0's 3rd party service such as Facebook, Auth0, Microsoft, Google, etc.

# Configure Auth0 Client

## Get an Auth0 account

[Create an account](https://auth0.com) with Auth0

## Create an Auth0 client

1. Log into Auth0 account
2. In the left side panel, click **Clients**
3. Click "**Create Client**" button
4. Choose "**Regular Web Application**"

This will now drop you at the quick start page for your client. Proceed with the following steps to configure your new client

1. Click the **Settings** tab in your new client
2. 1. Add the following callback urls
   2. [https://yo.droplit.io/oauth2/callback](https://yo.droplit.io/oauth2/callback)
3. Set the token endpoint authentication method to **POST**
4. Make sure to save your changes by clicking the "**Save Changes**" button at the bottom of the page

### Enable Service Providers

This is necessary if you are aiming to allow users to authentication with 3rd party providers such as Facebook and Twitter.

1. In the left site panel, click **Connections **then from the drop down, **Social**
2. Enable your providers as you see it and configure to your provider's settings

### Install Droplit Auth0 Skill

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



