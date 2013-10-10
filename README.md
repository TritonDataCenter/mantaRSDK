mantaRSDK
=========

Joyent Manta R Software Development Kit

UNDER CONSTRUCTION

Currently assumes you have a working Manta account, Unix/Linux R, 
environment variables set for Manta use, and openSSL installed and
working. If you have the Node.js manta CLI working, this should work.


### From R ###


Install:
```
install_packages(devtools)
library(devtools)
install_github("mantaRSDK,username="cwvhogue")
```

Test:
```
library(mantaRSDK)
help(mantaRSDK)
mantaAttempt()
```

Remove:
```
library(mantaRSDK)
detach(package:mantaRSDK, unload=TRUE)
```
