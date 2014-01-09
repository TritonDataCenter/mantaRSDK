mantaRSDK
=========

Joyent Manta R Software Development Kit

STILL - UNDER CONSTRUCTION

System Requirements
====================
To use this you need a working Manta account, 
Unix/Linux/Windows R v3.0 or up, as well as
environment variables set for Manta use, 
and openSSL installed and working. 

If you have the Node.js manta command line tools working on a Unix/Linux
platform, this should work as is. 

Windows has specific requirements, detailed below.

Getting Started with Manta
==========================

1) Create a Joyent Cloud account at http://www.joyent.com

2) Create a private/public ssh key pair using the Joyent
web console, as prompted when you sign up. Copy and save 
the value of your SSH key id which will look something like this:  
91:b4:d2:34:f1:b8:3c:1b:11:d1:b7:6c:e4:3c:4e:01 

3.1) UNIX (WINDOWS - skip ahead and do steps 3.3 - 3.5 )
Put the private key file and public key files into a .ssh 
directory located in your home directory/folder. Private key
is named 'id_rsa' and public key is named 'id_rsa.pub'

mantaRSDK will look for the Unix SSH key pair here:
```
~/.ssh/id_rsa
~/.ssh/id_rsa.pub
```

3.2) UNIX: ENVIRONMENT VARIABLES add these lines to ~/.bashrc with
a text editor:
```
export MANTA_URL=https://us-east.manta.joyent.com
export MANTA_USER=yourusername
export MANTA_KEY_ID=91:b4:d2:34:f1:b8:3c:1b:11:d1:b7:6c:e4:3c:4e:01 
```
then source the file:
```
source ~/.bashrc
```


3.3) WINDOWS:
Put the Joyent generated private key file and public key files into a .ssh 
directory located in your home directory/folder. Private key
is named 'id_rsa' and public key is named 'id_rsa.pub'. 

Windows -SSH key pair should be in your account folder "youraccount":
```
C:\Users\youraccount\.ssh\id_rsa
C:\Users\youraccount\.ssh\id_rsa.pub
```

3.4) WINDOWS - Install the free Windows version of OpenSSL from 
http://slproweb.com/download/Win64OpenSSL-1_0_1e.exe. Make note of
the folder path for openssl.exe to add to your system PATH in the
next step.

3.5) WINDOWS - Environment Variables and PATH
Set the environment variables from an administrator account as follows:
Run a command prompt as administrator. Click on "Start" button, type cmd,
right click on cmd program icon and select "Run as administrator".
Launch System Properies by issuing the command "start sysdm.cpl".
From the System Properties dialogue box, select tab "Advanced" then button
at the bottom "Environment Variables..."
Use the Environment Variables dialogue box to add in these three, supplying
your own values for MANTA_KEY_ID and MANTA_USER:
 
```
Variable        Value
MANTA_KEY_ID    91:b4:d2:34:f1:b8:3c:1b:11:de:b7:6c:e4:3c:4e:01
MANTA_URL       https://us-east-manta.joyent.com
MANTA_USER      username
```

Edit the User variable 'PATH' by clicking on "Edit", hit the END key on your keyboard
 and type ";" then the full openssl.exe path you got from step 3.4) 
so it is added to the end of the Path like this:
```
...;C:\bin\OpenSSL-Win64\bin
```

Click the Environment Variables dialogue box "OK" button and then 
the "Systems Properties" "OK" button.



4) UNIX/LINUX/WINDOWS Optional (and useful for debugging your SSH key install). 
Download and install Node.js at http://nodejs.org/download. Install the Node.js 
package called "manta" with the command "npm install -g manta" from a shell
or command prompt. Try the command 'mput  to see if you can put a simple
text file on manta and list your manta directory as follows:

Unix or Windows command line prompt:
```
echo "hello manta" > hello.txt
mput -f hello.txt ~~/stor
mls
```

If this is not working, check to see your enviroment variables are set
with the command "env | grep MANTA" on Unix, or "set" on Windows.

Once you have Manta working from the command line you are ready to
install mantaRSDK into R:


Installing the mantaRSDK package in R
=====================================


### mantaRSDK Installation
The mantaRSDK uses install_github() which requires the Rtools package:

http://cran.r-project.org/bin/windows/Rtools

### From R ###


Install:
```
install.packages("devtools")
library(devtools)
install_github("bunyan", username="cwvhogue")
install_github("mantaRSDK", username="cwvhogue")
```

Test:
```
library(mantaRSDK)
#help(mantaRSDK)
mantaWhoami(all=TRUE)
mantaLs.l()
mantaSetwd.public()
mattaGetwd()
mantaLs.url()
```

Remove:
```
library(mantaRSDK)
detach(package:mantaRSDK, unload=TRUE)
```


###  Manta Account Management
```
     mantaAccount() mantaWhoami() mantaGetLimits() mantaSetLimits()
```

###  Manta Hierarchical Directory Operations

```
     mantaGetwd() mantaSetwd() mantaSetwd.jobs() mantaSetwd.public()
     mantaSetwd.reports() mantaSetwd.stor() mantaSetwd.ws()
     mantaMkdir() mantaRmdir() mantaLs() mantaLs.du() mantaLs.l()
     mantaLs.n() mantaLs.paths() mantaLs.url() mantaFind()
     mantaFind.du() mantaFind.l() mantaFind.n() mantaFind.sizepath()
     mantaFind.sizes() mantaFind.url()
```

###  Manta Object Store Operations

```
     mantaExists() mantaPut() mantaGet() mantaCat() mantaRm()
     mantaSnapln() mantaDump() mantaSource() mantaSave() mantaLoad()
     mantaSave.ws() mantaLoad.ws()
```

###  Manta Compute Job Operations

```
     mantaJob.setup() mantaMap() mantaReduce() mantaJob.launch()
     mantaJob.status() mantaJob.done() mantaJob.cancel()
     mantaJob.errors() mantaJob.errors.stderr() mantaJob.failures()
     mantaJob.inputs() mantaJob.outputs() mantaJob.outputs.cat()
     mantaJobs() mantaJobs.running() mantaJobs.tail()
```

###  Exposed Low Level Calls
```
     mantaAttempt() mantaXfer() mantaSave.image()
```

###  Useful Bunyan Debug/Log Utilities
```
     bunyanSetLog() bunyanBuffer() bunyanTraceback()
```

