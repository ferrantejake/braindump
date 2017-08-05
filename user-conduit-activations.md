# User Conduit Activations
If you are considering using devices such as Nest, Ecobee, and other cloud-based devices then you are going to need access to the respective provider's cloud system to allow users to log in and control their cloud-based device. This is where Droplit conduits become useful. Conduits allow communication between the Droplit system and another cloud service.

In this document, we will walk through configuring your application to allow users to setup and configure their cloud-based devices. We will do this by creating a new user and environment and authorizing Droplit access to the devices. 

# Requirements
- A configured conduit
- A HTTP Client (i.e. Fiddler, Postman) or access to a terminal with cURL installed.
If you have not already done so, make sure you configure your respective conduit, i.e. Nest, Ecobee. Vist <insert link to conduits page> for a list of supported conduits and to view how to configure them.

You will be able to follow this simply using the Droplit Portal and a terminal interface. Also included are JavaScript resources to allow you to integrate this into your application.  

# Setting Up A Demo Environment/User
_It is reccommended to create a demo user and environment for this walkthrough. We do this by first creating a new demo user and a demo environment and granting the demo user access to the demo environment. _

First we must choose an environment for the user to access. Navigate to [the environments panel](https://portal.droplit.io/environments) and choose a pre-existing environment or create a new environment. Copy the _ID_ field from the environment record. We will use this to grant the demo user access.

Now we much choose a user the cloud device will be activated for. Navigate to [the users panel](https://portal.droplit.io/users) select a pre-existing user or click _Create a user_. If you create a new user, choose a unique id and an email to assign to this user. In this example, we will use `droplitdemo|123`.
> When creating demo users, the convention we prefer is `<service>|<uniqueId>`. This allows us to namespace users to certain services, as to ensure uniqueness amongst users. For example, if our uniqueId were without a namespace, two users with the same uniqueId could collide. This of course would not be ideal.

We must assign access to an environment for this user. Under _Access_, click the _New access_ button. Paste the environment id we copied from our demo environment earlier. This will grant the user access to the environment.

# Request Activation
In this section, we will use cURL and JavaScript to make calls to the Droplit API both as a developer and as a user. With the appropriate credentials in the request, the Droplit API will respond with a url for the application to redirect to, allowing the user to log into the respective service. Once logged in, the user is able to add devices to the environment specified in initial request for the redirect url.

##  Request Activation As A User
This is the typical use case. In this scenario, your application will be making these requests on behalf of a logged in user. Following the redirect will take the user to the specified service and allow them to enter their credentials to authorize Droplit to access the cloud service specified in the original request by the application.

Because we expect the user to be logged in, this requires a user token. To retrieve a user token, navigate to the user in the Droplit portal and under the _Actions_ section click _Generate token_ to generate a new token. This token will be used to initiate a conduit activation session on the user's behalf.

Make the following request and fill in the variables appropriately.
### Make Request Using cURL
```
 curl --h "Authorization: AUTH_TOKEN" -h "Content-Type: application/json" https://ioe.droplit.io/api/ecosystems/ECOSYSTEM_ID/conduit/CONDUIT_NAME/activate?environmentId=ENVIRONMENT_ID"
```
### JavaScript
```javascript
$.ajax({
    dataType: "json",
    headers: {
      'Authorization': AUTH_TOKEN,
      'Content-Type': 'application/json'
    }
    url: 'https://ioe.droplit.io/api/ecosystems/ECOSYSTEM_ID/conduit/CONDUIT_NAME/activate?environmentId=ENVIRONMENT_ID',
    success: function(data) {
    	var response = JSON.stringify(data, null, 2);
    	console.log(response);
    }
});
```

##  Request Activation As A Developer
This use case should only be employed if a developer is creating a development user account and using it for development purposes. We do not reccommend activating non-development user accounts with a developer account as a general practice.

 Here we will require a developer token. To retrieve your developer token, you must be logged into the Droplit portal. In the top right of the portal, click your username for a drop down and _Manage Account_. In the pop-up under _Authorization Info_ click the _Show_ button to view your developer token.

### Make Request Using cURL
```
 curl --h "Authorization: AUTH_TOKEN" -h "Content-Type: application/json" https://ioe.droplit.io/api/ecosystems/ECOSYSTEM_ID/conduit/CONDUIT_NAME/activate?environmentId=ENVIRONMENT_ID&uniqueId=UNIQUE_ID"
```
### JavaScript
```javascript
$.ajax({
    dataType: "json",
    headers: {
      'Authorization': AUTH_TOKEN,
      'Content-Type': 'application/json'
    }
    url: 'https://ioe.droplit.io/api/ecosystems/ECOSYSTEM_ID/conduit/CONDUIT_NAME/activate?environmentId=ENVIRONMENT_ID&uniqueId=UNIQUE_ID',
    success: function(data) {
    	var response = JSON.stringify(data, null, 2);
    	console.log(response);
    }
});
```

## Expected Response
In either case, the expected response is of the following format:
```json
{
  redirect: "https://dev-yo.droplit.io/auth/activate?session=gL…rBhTcPRWHmURBc7sWP5xsTJocFLqr2P-LcTGCT-DxL1Q20TjG" 
}
```

Once redirected, the user will be able to sign into their service and authorize Droplit to access their devices. Once this occurs, the devices will be available for consumption in the Droplit API. 