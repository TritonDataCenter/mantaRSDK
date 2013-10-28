mantaRSDK
=========

Joyent Manta R Software Development Kit

UNDER CONSTRUCTION

Currently you need a working Manta account, 
Unix/Linux/Windows R, environment variables set for Manta use, 
and openSSL installed and working. 
If you have the Node.js manta CLI working, this should work.


### From R ###


Install:
```
install.packages(devtools)
library(devtools)
install_github("mantaRSDK", username="cwvhogue")
```

Test:
```
library(mantaRSDK)
help(mantaRSDK)
mantaWhoami(all=TRUE)
mantaAttempt()
```

Remove:
```
library(mantaRSDK)
detach(package:mantaRSDK, unload=TRUE)
```
