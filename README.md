mantaRSDK
=========

Joyent Manta R Software Development Kit 0.8.0


System Requirements
====================
To use this you need a working Manta account, 
Unix/Linux/Windows R v3.0 or up, as well as
environment variables set for Manta use, 
and openSSL installed and working. 

If you have the Node.js Manta Command Line Tools working on a Unix/Linux
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

3.2) UNIX: ENVIRONMENT VARIABLES add these lines to ~/.bashrc or
~/.bash_profile with a text editor, substituting your username.
```
export MANTA_URL=https://us-east.manta.joyent.com
export MANTA_USER=yourusername
export MANTA_KEY_ID=91:b4:d2:34:f1:b8:3c:1b:11:d1:b7:6c:e4:3c:4e:01 
```
then source the appropriate file:
```
source ~/.bash_profile  
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

3.4) WINDOWS - Install the free Windows 64 bit version of OpenSSL from 
http://slproweb.com/download/Win64OpenSSL-1_0_1f.exe . Make note of
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
MANTA_USER      yourusername
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

### Testing From R ###


Install:
```
install.packages("devtools")
library(devtools)
install_github("Rbunyan", username="joyent")
install_github("mantaRSDK", username="joyent")
```

Test:
```
library(mantaRSDK)
?mantaRSDK
mantaWhoami(all=TRUE)
mantaLs.l()
mantaSetwd.public()
mattaGetwd()
# Work through Examples in these key Help pages
?mantaSetwd
?mantaMkdir
?mantaPut
?mantaGet
?mantaSave.ws
?mantaJob.launch
```

Remove:
```
library(mantaRSDK)
detach(package:mantaRSDK, unload=TRUE)
```

mantaRSDK Functions
===================


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

More Function Detail
====================


###     Manta Account Management

```
     mantaAccount()
```

Changes current Manta account information.

```
     mantaWhoami()
```

Report the active Manta account settings.

```
     mantaGetLimits()
```

Returns Manta durability level, connection timeouts and limits
currently active.

```
     mantaSetLimits()
```

Sets Manta durability level, connection timeouts and limits
currently active.

###     Manta Hierarchical Directory Operations

```
     mantaGetwd()
```

Gets Manta working directory.

```
     mantaSetwd()
     mantaSetwd.public()
     mantaSetwd.stor()
     mantaSetwd.ws()
     mantaSetwd.jobs()
     mantaSetwd.reports()
```

Sets Manta working directory. Dotted forms are top-level (public,
stor, jobs, reports) or workspace (as set by mantaSave.ws).

```
     mantaMkdir()
```

Makes a Manta subdirectory, optionally with parent directories.

```
     mantaRmdir()
```

Removes a Manta subdirectory.

```
     mantaLs()
     mantaLs.du()
     mantaLs.l()
     mantaLs.n()
     mantaLs.paths()
     mantaLs.url()
```

Lists, searches, filters, sorts and formats Manta directory
listings. Dotted forms alter the format of the output.  Numerical
values are returned by n (number) and du (disk used).

```
     mantaFind()
     mantaFind.du()
     mantaFind.l()
     mantaFind.n()
     mantaFind.sizepath()
     mantaFind.sizes()
     mantaFind.url()
```

Recursive find tool for retrieving matching objects/subdirs from
Manta hierarchy.  Dotted forms alter the format of the output.
Numerical values are returned by n (number) and du (disk used).


###     Manta Object Store Operations

```
     mantaExists()
```

Tests to see if a Manta object or subdirectory exists.

```
     mantaPut()
```

Uploads file(s) (vectorized), or raw R buffer data to Manta
Storage Service.

```
     mantaGet()
```

Downloads Manta object(s) (vectorized) specified to file(s) or
buffer.

```
     mantaCat()
```

Retrieves object from Manta and uses cat() to print contents to
the R console.

```
     mantaRm()
```

Removes specified Manta object, optionally recursive.

```
     mantaSnapln()
```

Makes a Snaplink - combination snapshot and symbolic link.

```
     mantaDump()
```

Uses dump() to upload text parsable R data to Manta Storage
Service.

```
     mantaSource()
```

Downloads specified Manta object and applies source() to parse R
code file.

```
     mantaSave()
```

Uploads R data to Manta Storage Service using R function save().

```
     mantaLoad()
```

Downloads specified Manta object containing R data and uses R
function load().

```
     mantaSave.ws()
```

Saves R workspace to Manta R workspace directory with an audit
trail of backups.

```
     mantaLoad.ws()
```

Loads last current R workspace from Manta R workspace directory.

###   Manta  Compute Job Operations

```
     mantaJob.setup()
     mantaMap()
     mantaReduce()
```

Constructors for R format Manta Job including name, and UNIX
command line tasks as defined by mantaMap(), and/or mantaReduce()
functions.

```
     mantaJob.launch()
```

Submits list of input Manta objects and R format Manta Job
specification, runs job optionally polls job status. Returns job
status.

```
     mantaJob.cancel()
```

Sends Manta a cancel message to stop running job.

```
     mantaJob.status()
```

Returns JSON Manta job status data given Manta job identifier.

```
     mantaJob.done()
```

Checks or polls status of a Manta job.  Returns done or not as
logical.

```
     mantaJob.errors()
```

Returns JSON Manta error messages given Manta job identifier.

```
     mantaJob.errors.stderr()
```

Retrieves JSON errors given Manta job identifier, then retrieves
each stderr message archived on Manta (if any) and uses mantaCat()
to print contents of stderr to the console.

```
     mantaJob.failures()
```

Returns list of failures given Manta job identifier.

```
     mantaJob.inputs()
```

Returns list of input Manta objects given Manta job identifier.

```
     mantaJob.outputs()
```

Returns list of output Manta objects given Manta job identifier.

```
     mantaJob.outputs.cat()
```

Retrieves list of Manta output objects given Manta job identifier,
then retrieves each object from Manta and uses cat() to print
contents to the R console.

```
     mantaJobs()
```

Lists all Manta job identifiers, sorted by time.

```
     mantaJobs.running()
```

Lists identifiers of any running Manta jobs.

```
     mantaJobs.tail()
```

Returns identifier of last run Manta job identifier, or from
offset n up from end of list.

###     Exposed Low Level Calls

```
     mantaAttempt()
```

raw REST API Manta Caller with exception handling, used by many
functions.

```
     mantaXfer()
```

raw REST API Manta Caller for mantaPut() mantaGet() and related
data transfer routines.

```
     mantaSave.image()
```

Workspace Upload function that calls R save.image(); used by
mantaSave.ws().

###     Useful Bunyan Debug/Log Utilities

```
     bunyanSetLog()
```

Starts bunyan JSON message logging at supplied logging threshold
to file or memory buffer.

```
     bunyanBuffer()
```

Returns memory buffer.

```
     bunyanTraceback()
```

Get messages from memory after last bunyanSetpoint

