\name{mantaFind.du}
\alias{mantaFind.du}
\title{Recursive find tool for retrieving matching objects/subdirs from Manta hierarchy.
Output is disk utilized in bytes NOT counting redundancy levels.}
\usage{
mantaFind.du(mantapath, grepfor, entries, items = "o", level = 0, starttime,
  endtime, ignore.case = FALSE, perl = FALSE, verbose = FALSE,
  info = TRUE)
}
\arguments{
  \item{mantapath}{character, optional. Current
  subdirectory set by \code{mantaSetwd} is used, otherwise
  specify full Manta path to subdirectory. Supports
  \code{~~} expansion to your Manta username, e.g.
  \code{"~~/public"} and UTF-8 encoded characters.}

  \item{grepfor}{character optional. Regular expression for
  \code{grep} name search. Ignored for reprocessed trees.
  Uses R regexps, N.B. use \code{"[.]txt"}, not
  \code{"*.txt"} to match filename extensions.}

  \item{entries}{optional. Saved mantaFind R data. For
  reprocessing/reformatting retrieved R tree information
  saved with \code{mantaFind(l='R')->tree}.}

  \item{items}{character optional. \code{'a'} for all,
  \code{'d'} for directory, \code{'o'} for object.}

  \item{level}{integer optional. Maximum number of
  subdirectory child levels to visit, in other words, the
  depth of the hierarchical directory search. If
  \code{level <= 0}, search depth is unrestricted. Level
  parameter is ignored on reprocessed search trees.}

  \item{starttime}{POSIXlt time, optional. Start time for
  time-bounded find. When used without \code{endtime},
  \code{endtime} is set to current UTC time.}

  \item{endtime}{POSIXlt time, optional. End time for
  time-bounded find. When used without \code{starttime},
  \code{starttime} is set to start of Manta service}

  \item{ignore.case}{logical, optional. Argument passed to
  R \code{grep} for searching.}

  \item{perl}{logical, optional. Argument passed to R
  \code{grep} for searching.}

  \item{verbose}{logical, optional. Verbose RCurl HTTP data
  output on Unix.}

  \item{info}{logical, optional. Console status messages
  about child path progress.}
}
\description{
Search for object or directory names with regular
expressions (using R grep). Reports disk size.
}
\seealso{
\code{\link{mantaLs.du}}

Other mantaFind: \code{\link{mantaFind.l}};
\code{\link{mantaFind.n}};
\code{\link{mantaFind.sizepath}};
\code{\link{mantaFind.sizes}}; \code{\link{mantaFind.url}};
\code{\link{mantaFind}}
}

