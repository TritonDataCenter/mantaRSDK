\name{mantaSnapln}
\alias{mantaSnapln}
\title{Makes a Snaplink - combination ZFS snapshot and Symbolic link.}
\usage{
  mantaSnapln(from, to, info = TRUE)
}
\arguments{
  \item{from}{character, required. Object in current
  subdirectory or full Manta path to stored object.
  Vectorized.}

  \item{to}{character, required. Snaplink name in current
  subdirectory, existing Manta subdirectory or full Manta
  object path to the new SnapLink. If \code{from} is a
  vector of Manta paths, then \code{to} must specify a
  single valid Manta subdirectory.}

  \item{info}{logical. When FALSE suppresses messages on
  the console.}
}
\description{
  As a persistent object store, there are no copy or move
  commands on Manta. Instead the \code{mantaSnapln} command
  is used to add an object's name into another subdirectory
  without physically moving data on the service. Internally
  the system takes a ZFS snapshot of the data and the new
  object entry is the snapshot. If the original data is
  overwritten, the SnapLink still points to the original
  snapshot. The R workspace audit trail used by
  \code{\link{mantaSave.ws}} and \code{\link{mantaLoad.ws}}
  is implemented using \code{mantaSnapln}.
}
\examples{
\dontrun{
## Save a static hello world HTML page
htmlpage <- paste("<!DOCTYPE html>\\n<html>\\n<body>\\n\\n",
                  "<h1>Hello from Joyent Manta.</h1>\\n\\n",
                  "<p>Hello world! from ",
                  mantaWhoami(),
                  " .</p>\\n\\n",
                  "</body>\\n</html>", sep="")
file <- file("index.html", "wb")
write(htmlpage,file)
close(file)
rm(file)

## Upload the HTML file to Manta in your private area
mantaSetwd.stor()
mantaPut("test_index.html")
mantaCat("test_index.html")

## Make it public
mantaMkdir("~~/public/test")
mantaSnapln("test_index.html", "~~/public/test/index.html")

## copy and paste URL into browser.
mantaLs.url("~~/public/test", grepfor = "index.html")

## Delete the original in private area
mantaRm("~~/stor/test_index.html")
mantaExists("~~/stor/test_index.html")

## Snaplink copy is still there public
mantaExists("~~/public/test/index.html")
mantaCat("~~/public/test/index.html")
}
}
\seealso{
  \code{\link{mantaSave.ws}} \code{\link{mantaLoad.ws}}

  Other Directory: \code{\link{mantaGetwd}},
  \code{\link{mantaMkdir}}, \code{\link{mantaRm}},
  \code{\link{mantaRmdir}}, \code{\link{mantaSetwd}},
  \code{\link{mantaSetwd.public}},
  \code{\link{mantaSetwd.reports}},
  \code{\link{mantaSetwd.stor}}
}
\keyword{Manta,}
\keyword{manta}

