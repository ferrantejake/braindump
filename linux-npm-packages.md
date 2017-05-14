# Installing Global NPM Packages in a Restricted Linux Environment

_disclaimer: I stole this from Chris Woodle per his recommendation, thus all credit goes to him. I am putting this here for persistence sake_

---

There are a few ways to solve this issue according to [docs.npmjs.com](https://docs.npmjs.com/getting-started/fixing-npm-permissions). We prefer **Option 2** as it does not require changing directory ownership, which may be a security risk.

First, change the install location of global packages.

```
mkdir ~/.npm-global
npm config set prefix '~/.npm-global'
```

Finally, create/edit `.bashrc` to add the new directory to your `PATH`. 

```
echo "export PATH=~/.npm-global/bin:\$PATH" >> ~/.bashrc
```

Restart your terminal and you should be able to install and use global packages. 



