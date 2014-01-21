\name{mantaPut}
\alias{mantaPut}
\title{Uploads file(s) (vectorized), or raw R buffer data to Manta Storage Service.}
\usage{
mantaPut(filename, mantapath, buffer, md5 = FALSE, headers, info = TRUE,
  verbose = FALSE)
}
\arguments{
  \item{mantapath}{character, optional. Path to where
  uploaded data will go on Manta or Manta Object file name
  in current working Manta directory. If \code{mantapath}
  ends in "/" it is assumed to be specifying a Manta
  subdirectory and the filename portion is appended to it.
  Memory data uploads using \code{buffer} parameter require
  \code{mantapath} to have a destination file name at the
  end of the path with an extension for proper guessing of
  \code{Content-Type} header information.}

  \item{filename}{vector of character, optional. Path to
  local file to upload. If only a filename is given,
  assumes file is in path specified by \code{getwd}.
  Vectorized.}

  \item{buffer}{optional. Raw buffer of data to put. If
  filename is specified, buffer is ignored and filename
  contents are uploaded. Not vectorized.}

  \item{md5}{logical optional. Test md5 hash of file/raw
  buffer with OpenSSL before/after upload. \code{TRUE} by
  default, setting \code{FALSE} will speed up transfers a
  bit.}

  \item{headers}{optional. Headers including R structured
  metadata (up to 4k in user metadata) as array of named
  character E.g.\cr \code{headers = c('content-type' =
  "image/jpg", 'x-durability-level' = 4)} Manta user
  metadata is prefixed with \code{"m-"}, E.g.\cr
  \code{headers = c('content-type' = "x-chemical/x-pdb",
  'm-molecule-class' = "protein")}}

  \item{info}{logical required.  \code{FALSE} silences
  output messages while downloading.}

  \item{verbose}{logical, optional. Passed to \code{RCurl
  GetURL}, Set to \code{TRUE} to see background REST
  communication on \code{stderr} Note this is not visible
  on Windows.}
}
\value{
\code{TRUE} or \code{FALSE} depending on success of upload.
}
\description{
Transfers file, buffer to Manta Storage Object specified in
mantapath. To save a file, specify the filename. It will go
into the current working Manta directory with the same name
unless absolute Manta path or relative object name is
specified with the \code{mantapath} argument. To see the
current directory on Manta use \code{\link{mantaGetwd}},
and \code{\link{mantaSetwd}} to change it.
}
\details{
To save a raw R buffer, pass the string with its name e.g.
buffer = "myRawBuffer"

One limitation of the \code{mantaRSDK} is that it is not
designed to handle large (multi-Gigabyte or larger)
objects. Uploads - \code{mantaPut} - work from files, but
Downloads - \code{\link{mantaGet}} - fill the R memory
space to their completion before being written to a file.
To download files larger than your R memory, use the Manta
Node.js command line tool \code{mget}. The Node.js based
Manta command line tools employ streams, so object size is
not a limitation.

The Content-Type information metatdata for the file is
obtained using the RCurl library function
\code{guessMIMEtype} on the filename extension provided,
e.g. \code{.jpg} or it can be set by passing a
\code{header} parameter, which is an \code{RCurl} style
HTTP header - a list of named character values like
this:\cr \code{header = c('content-type' = "image/jpg")}.
The default \code{Content-Type} header is
\code{"application/octet-stream"}.  The number of copies
(durability level) saved is by default 2. This can be
changed by using headers like this: \cr \code{headers =
c('x-durability-level' = 4)} for one-time use.\cr For
larger operations use \code{\link{mantaSetLimits}} to
change the default durability level for the current
mantaRSDK session. The number of copies stored can range
from 2 to 6. This function does not support streaming
uploads, for that use the Node.js Manta command line
interface (CLI). Other Manta operations involving HTTP
conditional request semantics and CORS headers are not
implemented in this R library, but can be used with the
Node.js CLI which can be called from R with the
\code{system2} command shell. \code{mantaPut} is a wrapper
for \code{mantaXfer}, which implements the actual
\code{RCURL} data upload.
}
\examples{
\dontrun{
## Save a static hello world HTML page
htmlpage <- paste("<!DOCTYPE html>\\n<html>\\n<body>\\n\\n",
                  "<h1>Hello from Joyent Manta.</h1>\\n\\n",
                  "<p>Hello world! from ",
                  mantaWhoami(),
                  ".</p>\\n\\n",
                  "</body>\\n</html>", sep="")
file <- file("test_index.html", "wb")
write(htmlpage,file)
close(file)
rm(file)
rm(htmlpage)

## Upload the HTML file to Manta in your public area
mantaSetwd.public()
mantaPut("test_index.html")
mantaExists("test_index.html")
mantaCat("test_index.html")

ls()
buffer <- mantaGet("test_index.html", buffer = TRUE)
cat(rawToChar(buffer))

## Upload the raw buffer
mantaPut(mantapath = "~~/public/buffer_index.html", buffer = buffer)
mantaLs.l(grepfor = "[.]html")
mantaCat("buffer_index.html")

## Check file metadata to see Content-type
mantaGet("test_index.html", metadata = TRUE)
mantaGet("buffer_index.html", metadata = TRUE)

## copy and paste URL into browser.
mantaLs.url("~~/public", grepfor = "[.]html")

## Cleanup this demo
#mantaRm("~~/public/test_index.html")
#mantaRm("~~/public/buffer_index.html")
#file.remove("test_index.html")
#rm(buffer)
}
}
\seealso{
\code{\link{mantaGet}}

Other mantaPut: \code{\link{mantaDump}};
\code{\link{mantaSave.image}}; \code{\link{mantaSave.ws}}
}
\keyword{Manta}

