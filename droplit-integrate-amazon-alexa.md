# Amazon Alexa Skill Droplit Integration

# Overview

This walkthrough shows you how to integrate Alexa voice services with your Droplit account. This consists of the following steps:

* Create/Configure Alexa skill \(AWS Developer Dashboard\)
* Create/Configure Droplit Alexa skill conduit \(Droplit Dashboard\)
* Configure Droplit Alexa skill conduit to communicate with Auth0 client 

Once this is completed, users in your ecosystem will be able to use Alexa voice services though devices like the Echo, Echo dot, or services like [Echosim](https://echosim.io/welcome?next=%2F) wherever Alexa voice services are supported, to control devices on the Droplit platform.

# Prerequisites

* A working Amazon Alexa skill
* Amazon Alexa Developer account
* Amazon Web Services account
* Amazon Alexa App
* A configured Identity Service Provider 
* Droplit account
* Droplit Ecosystem
* Droplit Environment \(only required for testing\)

If you have not configured an identity service provider, then take a look at [this article](https://ferrantejake.gitbooks.io/braindump/content/droplit-installing-identity-provider-conduit.html) for more information on setting up your Droplit acount with Auth0 as your identity service provider.

## Configure Amazon Alexa With Droplit

### Create Amazon Alexa Skill

1. Sign into your Amazon Developer account [https://developer.amazon.com](https://developer.amazon.com)
2. Once in the developer console, click the Alexa tab and navigate to Alexa Skills Kit 
3. Whether you are creating a new skill or integrating a pre-existing one, make sure it is configured to use the Smart Home Skill API. This option can be configured under `Skill Infoirmation>Skill Type` in your skill's configuration settings
   1. We will come back to this, so make sure to keep the tab open and save your work

### Install Droplit Amazon Alexa Skill

1. In a new tab go to [http://portal.droplit.io](http://portal.droplit.io) and sign in
2. Click "Conduits" \(side-left panel\) to open the conduits view
3. Find and install the Alexa Smart Home Skill conduit
   1. Name your conduit aptly - you have one per ecosystem
4. Open open your newly installed Amazon Alexa Skill conduit
   1. Here we should see some documentation for the conduit's settings, incluiding the `Authorization URL`, `Client Id`, and others.

### Configure Alexa for Droplit

#### Alexa Configuration Dashboard

1. Use the information from the Droplit Alexa Skill conduit configuration to setup the following fields in the Amazon Developer Console Alexa Skill configuration
   1. Authorization URL
   2. Client Id
   3. Domain List
      1. An important note: these are the domains associated with Droplit which will be visited during the authorization process. If one needs to authenticate with another service such as Auth0 and/or Facebook, then they will require the domains associated with making that transaction.
   4. Scope
   5. Access Token URI
   6. Client Secret
2. Complete the following:
   1. Select **Auth Code Grant** for Authorization Grant Type
   2. Select **HTTP Basic** for Client Authentication Scheme

#### Choose Your Lambda Function

You can choose to use Droplit's \(recommended\) provided Lambda function, or you can use your own. For more information on this, visit &lt;Configure Droplit AWS Lambda Function&gt;. In the mean time, we can use the provided Droplit AWS Lambda function

**Droplit Lambda Function**: _arn:aws:lambda:us-east-1:428766893289:function:droplitSmartHomeAdapter_

#### Droplit Alexa Skill

1. Use the information from the Amazon Developer Console Alexa Skill configuration to set up the following fields in the Droplit Alexa Skill configuration
   1. Redirect URL
      1. There are two URLs provided in the Amazon Developer Dashboard Alexa Configuration. Use the URL which appears similar to the following:
         1. [https://pitangui.amazon.com/api/skill/link/M2NGQSTDAAUR23](https://pitangui.amazon.com/api/skill/link/M2NGQSTDAAUR23)
   2. Alexa Skill ARN
      1. This field requires the **Application ID** located in the **Skill Information **page on the Amazon Developer Dashboard.



