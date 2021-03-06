# Install Droplit Auth0 Conduit

# Overview

This walkthrough shows you how to install the Dropliot Auth0 conduit, set up a compatible Auth0 client, and configure the two to work together to provide authentication for your ecosystem. It consists of 3 parts:

* Create/Configure Auth0 client
* Install Droplit Auth0 conduit
* Configure Droplit Auth0 conduit to communicate with the Auth0 client 

Once this is completed, users in your ecosystem will be able to authenticate through either Auth0 or one of Auth0's 3rd party services such as Facebook, Microsoft, Google, etc.

# Setup Auth0 Client

An Auth0 client enables you to handle user logins through either Auth0's identity provider system or alternatively through once of the available 3rd party identity provider systems.

## Get an Auth0 account

[Create an account](https://auth0.com) with Auth0

## Create an Auth0 client

1. Log into Auth0 account
2. In the left side panel, click **Clients**
3. Click **Create Client** button
4. Choose **Regular Web Application**

This will now drop you at the quick start page for your client. Proceed with the following steps to configure your new client

1. Click the **Settings** tab in your new client
2. Set the token endpoint authentication method to **POST**
3. Make sure to save your changes by clicking the **Save Changes** button at the bottom of the page

## Enable Service Providers

Enabling service providers allows users to authenticate with 3rd party services such as Facebook and Twitter.

1. In the left site panel, click **Connections **then from the drop down, **Social**
2. Enable your providers as you see it and configure to your provider's settings

# Install Droplit Auth0 Conduit

The Droplit Conduit facilitates communication between Droplit and the the Auth0 client. This allows users to authenticate with Auth0 through the Droplit system, and your app.

In a new tab, [open the Droplit Developer Portal](http://portal.droplit.io/)  and sign in

> Tip: Conduits are ecosystem-wide, so we do not need to select an environment

1. In the left side panel, click **Ecosystems **and select your desired ecosystem
2. Click **Conduits** to open the Conduits view
3. Click the Auth0 conduit and open the Auth0 conduit options
4. Click **Install**
5. Label your conduit aptly - you have one for this ecosystem!

> Tip: Names only appear in the Developer console, so this is more for internal benefit of knowing what the conduit is. Labels on the other hand are how you will references the conduit in the CLI so give this a easy to remember, concise name.

# Configure Auth0 Client With Droplit Auth0 Conduit

This step will connect your Droplit Auth0 conduit to your Auth0 client, allowing users to authenticate using this workflow.

1. Go back to your Auth0 client's **Settings** page and copy the following fields into the Droplit Auth0 conduit configuration
   1. Client Id
   2. Client Secret
   3. Domain
2. Save your Droplit Auth0 conduit configuration



