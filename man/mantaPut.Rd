\name{mantaPut}
\alias{mantaPut}
\title{Saves a file, or raw R buffer data to Manta Storage Service.}
\usage{
  mantaPut(filename, mantapath, buffer, md5 = FALSE,
    headers, info = TRUE, verbose = FALSE)
}
\arguments{
  \item{mantapath}{string, optional. Path to where uploaded
  data will go on Manta or Manta Object file name in
  current working Manta directory. If manapath ends in "/"
  or "\" it is assumed to be specifying a Manta
  subdirectory and the filename portion is appended to it.
  Memory data uploads using buffer parameter require
  mantapath to have a destination file name at the end of
  the path with an extension for proper guessing of
  Content-Type header information.}

  \item{filename}{vector of character, optional. Path to
  local file to PUT. If only a filename is given, assumes
  file is in path specified by getwd(). Vectorized.}

  \item{buffer}{optional. Raw buffer of data to put. If
  filename is specified, buffer is ignored and filename
  contents are uploaded. Not vectorized.}

  \item{md5}{logical optional. Test md5 hash of file/raw
  buffer before/after PUT transfer.}

  \item{headers}{optional. Headers including R structured
  metadata (up to 4k in user metadata) as array of named
  character E.g. headers = c('content-type' = "image/jpg",
  'x-durability-level' = 4) Manta user metadata is prefixed
  with "m-", E.g. headers = c('content-type' =
  "x-chemical/x-pdb", 'm-molecule-class' = "protein")}

  \item{info}{logical required. Set to FALSE to silence
  output messages while downloading.}

  \item{verbose}{logical, optional. Passed to RCurl GetURL,
  Set to TRUE to see background REST communication on
  stderr which is invisible on Windows}
}
\value{
  TRUE or FALSE depending on success of PUT transfer
}
\description{
  Transfers file, buffer to Manta Storage Object file
  specified in mantapath.
}
\details{
  To save a file, specify the filename. It will go into the
  current working Manta directory with the same name unless
  a Manta path or file name is specified with the mantapath
  argument. To see the current directory on Manta use
  mantaGetwd(), and mantaSetwd() to change it.

  To save a raw R buffer, pass the string with its name
  e.g. buffer = "myRawBuffer"

  The Content-Type information metatdata for the file is
  obtained using the RCurl library function guessMIMEtype
  on the filename.ext provided, or it can be set by passing
  a header parameter, which is an RCurl style HTTP header
  list of named character values like this: header =
  c('content-type' = "image/jpg"). The default Content-Type
  header is 'application/octet-stream'.  The number of
  copies saved is by default 2. This can be changed by
  using headers like this: headers = c('x-durability-level'
  = 4) for one-time use, or for larger operations use
  mantaSetLimits to change the default for the current
  mantaRSDK session. The number of copies stored can range
  from 2 to 6. This function does not support streaming
  uploads, for that use the Node.js Manta command line
  interface (CLI). Other Manta operations involving HTTP
  conditional request semantics and CORS headers are not
  implemented in this R library, but can be used with the
  Node.js CLI which can be called from R with the system2()
  command shell.

  mantaPut is a wrapper for mantaXfer, which implements the
  RCURL transfer
}
\keyword{Manta,}
\keyword{manta}

