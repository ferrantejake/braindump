# Integrate Amazon Alexa

## Scenario

---

You are have a service you are providing/creating and would like to control your devices using Amazon Alexa's voice control services. We can do that.

## Objective

---

Configure Droplit Amazon Alexa conduit to work with your Amazon Alexa account.

## Requirements

---

* A working Amazon Alexa skill
* Amazon Alexa Developer account
* Amazon Web Services account
* Amazon Alexa App
* A configured Identity Service Provider 
* Droplit account

If you have not configured an identity service provider, then take a look at [this article](https://ferrantejake.gitbooks.io/braindump/content/droplit-installing-identity-provider-conduit.html) for more information on setting up your Droplit acount with Auth0 as your identity service provider.

## Configure Amazon Alexa With Droplit

---

### Configure Amazon Alexa Skill

1. Sign into your Amazon Developer account [https://developer.amazon.com](https://developer.amazon.com)
2. Once in the developer console, click the Alexa tab and navigate to Alexa Skills Kit 



### Install Droplit Amazon Alexa Skill

1. In a new tab go to [http://portal.droplit.io](http://portal.droplit.io) and sign in
2. Click "Conduits" \(side-left panel\) to open the conduits view
3. Find and install the Alexa Smart Home Skill conduit
   1. Name your conduit aptly - you have one per ecosystem
4. Open open your newly installed Amazon Alexa Skill conduit
   1. Here we should see some documentation for the conduit's settings, incluiding the `Authorization URL`, `Client Id`, and others.

### Configure Alexa Skill

## Conlusion

---

Congratulations, you've completed setting up an Auth0 identity service provider. You can now authenticate with the Auth0 service.

