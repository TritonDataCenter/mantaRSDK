\name{mantaLs.l}
\alias{mantaLs.l}
\title{Lists, searches, filters, sorts and formats Manta directory listings
Output is long \code{ls -o} unix style of directory listing.}
\usage{
mantaLs.l(mantapath, grepfor, json, items = "a", sortby = "none",
  decreasing = FALSE, ignore.case = FALSE, perl = FALSE,
  verbose = FALSE)
}
\arguments{
  \item{mantapath}{character, optional. Current
  subdirectory set by \code{mantaSetwd} is used, otherwise
  specify full Manta path to subdirectory. Supports
  \code{~~} expansion to your Manta username, e.g.
  \code{"~~/public"} and UTF-8 encoded characters.}

  \item{grepfor}{character optional. Regular expression for
  \code{grep} name search. Uses R regexps, N.B. use
  \code{"[.]txt"}, not \code{"*.txt"} to match filename
  extensions.}

  \item{json}{optional. Input saved JSON data from
  \code{mantaLs(format='json')} used for reprocessing
  previously retrieved listings. Include previously
  specified mantapath if you wish to recover true paths.}

  \item{items}{character optional. \code{'a'} for all,
  \code{'d'} for directory, \code{'o'} for object.}

  \item{sortby}{character, optional. Specify \code{'none'},
  \code{'name'}, \code{'time'}, or \code{'size'}.}

  \item{decreasing}{logical, optional. Argument passed to R
  \code{order} for sorting.}

  \item{ignore.case}{logical, optional. Argument passed to
  R \code{grep} for searching.}

  \item{perl}{logical, optional. Argument passed to R
  \code{grep} for searching.}

  \item{verbose}{logical, optional. Verbose HTTPS
  \code{RCurl} data output on Unix.}
}
\description{
Used for listing Manta subdirectory. Uses current working
Manta directory or one supplied in \code{mantapath}.
Searches for filenames with regular expressions (using R
\code{grep}). Sorts listings by filename, time, or size.
}
\seealso{
\code{\link{mantaFind.l}}

Other mantaLs: \code{\link{mantaExists}};
\code{\link{mantaLs.du}}; \code{\link{mantaLs.n}};
\code{\link{mantaLs.paths}}; \code{\link{mantaLs.url}};
\code{\link{mantaLs}}
}
\keyword{Manta}

