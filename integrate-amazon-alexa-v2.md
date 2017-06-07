# Amazon Alexa Skill Droplit Integration

# Overview

This walkthrough shows you how to integrate Alexa voice services with your Droplit account. This consists of the following steps:

* Create/Configure Alexa skill \(AWS Developer Dashboard\)
* Create/Configure Droplit Alexa skill conduit \(Droplit Dashboard\)
* Configure Droplit Alexa skill conduit to communicate with Auth0 client 

Once this is completed, users in your ecosystem will be able to use Alexa voice services though devices like the Echo, Echo dot, or services like [Echosim](https://echosim.io/welcome?next=%2F) wherever Alexa voice services are supported, to control devices on the Droplit platform.

# Prerequisites

* Amazon Alexa Developer account
* Amazon Alexa App
* A configured Identity Service Provider
* Droplit Environment \(only required for testing\)

If you have not configured an identity service provider, then take a look at [configuring the Auth0 identity service provider conduit](https://ferrantejake.gitbooks.io/braindump/content/droplit-installing-identity-provider-conduit.html).

# Create Amazon Alexa Skill

The Amazon Alexa Skill serves as the means for Alexa to communicate with Droplit. Users are able to install this skill on their Alexa account via the Alexa app.

1. [Sign into your Amazon Developer account](https://developer.amazon.com)
2. Select the **Alexa** tab and navigate to the **Alexa Skills Kit** 
3. Whether you are creating a new skill or integrating a pre-existing one, make sure it is configured to use the **Smart Home Skill API**. This option can be configured under **Skill Information** **&gt;** **Skill Type** in your skill's configuration settings
4. Choose your skill's name, invocation name mark the following global fields as follow

| Audio Player | No |
| :--- | :--- |


We will come back to this, so make sure to keep the tab open and save your work

# Install Droplit Amazon Alexa Skill Conduit

The Droplit Amazon Alexa Skill serves as a means for Droplit to assist the Amazon Alexa Skill to authenticate with a selected identity service provider. An identity service provider can be selected in the Droplit dashboard. More information on [choosing an identity service provider](https://ferrantejake.gitbooks.io/braindump/content/droplit-installing-identity-provider-conduit.html).

> Tip: Name your conduit aptly - you have one per ecosystem

1. In a new tab [sign in to the Droplit Portal](http://portal.droplit.io)
2. In the left side panel select **Conduits** to open the conduits view
3. Find and install the **Alexa Smart Home Skill** conduit

# Configure Alexa Skill With Droplit Alexa Skill Conduit

Connecting these features will allow for Alexa-enabled devices to authenticate through Droplit with a chosen identity service provider. This requires information from both the Amazon Alexa skill configuration as well as the Droplit Alexa Skill Conduit configuration so it would be helpful to have these portals side-by-side.

Use the information from the Droplit Alexa Skill conduit configuration to setup the following fields in the Amazon Developer Console Alexa Skill configuration.

* Authorization URL
* Client Id
* Domain List
* Scope
* Access Token URL
* Client Secret

> Note: The domains in the domain list are the domains associated with Droplit which will be visited during the authorization process. If you need to authenticate with another service such as Auth0 and/or Facebook, then they will require the domains associated with making that transaction.

Complete the following:

1. Select **Auth Code Grant** for Authorization Grant Type
2. Select **HTTP Basic** for Client Authentication Scheme
3. Save your settings

# Choose Your Lambda Function

The Lambda Function is code which will be executed when a user makes a request using an Alexa-enabled device. This function will depict the business logic of how Alexa communicates with the Droplit API. You can choose to use Droplit's \(recommended\) provided Lambda function, or you can use your own. For more information on this, visit &lt;Configure Droplit AWS Lambda Function&gt;. In the mean time, we can use the provided Droplit AWS Lambda function

**Droplit Lambda Function**: _arn:aws:lambda:us-east-1:428766893289:function:droplitSmartHomeAdapter_

# Configure Droplit Alexa Skill Conduit With Alexa Skill

Use the information from the Amazon Developer Console Alexa Skill configuration to set up the following fields in the Droplit Alexa Skill configuration

* Redirect URL

> Note: There are two URLs provided in the Amazon Developer Dashboard Alexa Configuration. Use the URL which appears similar to the following: [https://pitangui.amazon.com/api/skill/link/M2NGQSTDAAUR23](https://pitangui.amazon.com/api/skill/link/M2NGQSTDAAUR23)

* Alexa Skill ARN

> Note: This field requires the **Application ID** located in the **Skill Information **page on the Amazon Developer Dashboard.

Make sure to save your settings