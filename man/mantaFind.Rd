\name{mantaFind}
\alias{mantaFind}
\title{Recursive find tool for retrieving matching objects/subdirs from Manta hierarchy.}
\usage{
  mantaFind(mantapath, grepfor, entries, l = "paths",
    items = "o", level = 0, sortby = "none", starttime,
    endtime, decreasing = FALSE, ignore.case = FALSE,
    perl = FALSE, verbose = FALSE, info = TRUE,
    findroot = 1)
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

  \item{l}{character optional.\cr Specifies listing output
  format by \code{'paths', 'n', 'du', 'R'}.\cr
  \code{'paths'} is a listing of full Manta object
  pathnames needed for \code{mantaJobs}.\cr \code{'l'} is a
  Unix-y listing style with full pathnames.\cr
  \code{'sizes'} is a listing of sizes in bytes, no
  pathnames.\cr \code{'size_path'} is a listing of size
  [space] path.\cr \code{'URL'} is a listing of the URLs
  (only for objects in \code{~~/public/}).\cr \code{'n'} is
  the number of entries found.\cr \code{'du'} is the number
  of bytes used by objects (not counting redundancy
  levels!).\cr \code{'R'} is the R object collected by
  \code{mantaFind} with \code{mtime} parsed, full path
  names. \code{mantaFind(l='R') -> tree} saves the
  directory tree for reprocessing with
  \code{mantaFind(mantapath, entries = tree, ...)}.}

  \item{items}{character optional. \code{'a'} for all,
  \code{'d'} for directory, \code{'o'} for object.}

  \item{level}{integer optional. Maximum number of
  subdirectory child levels to visit, in other words, the
  depth of the hierarchical directory search. If
  \code{level <= 0}, search depth is unrestricted. Level
  parameter is ignored on reprocessed search trees.}

  \item{sortby}{character, optional. Specify \code{'none',
  'name', 'time',} or \code{'size'.} Sorting selection is
  independent of time-bounded find.}

  \item{starttime}{POSIXlt time, optional. Start time for
  time-bounded find. When used without \code{endtime},
  \code{endtime} is set to current UTC time.}

  \item{endtime}{POSIXlt time, optional. End time for
  time-bounded find. When used without \code{starttime},
  \code{starttime} is set to start of Manta service}

  \item{decreasing}{logical, optional. Argument passed to R
  \code{order} for sorting.}

  \item{ignore.case}{logical, optional. Argument passed to
  R \code{grep} for searching.}

  \item{perl}{logical, optional. Argument passed to R
  \code{grep} for searching.}

  \item{verbose}{logical, optional. Verbose RCurl HTTP data
  output on Unix.}

  \item{info}{logical, optional. Console status messages
  about child path progress.}

  \item{findroot}{integer, internal. Indicates nested
  calls, not to be used.}
}
\description{
  Search for object or directory names with regular
  expressions (using R grep). Sort listings by filename,
  time, or size. Report entries within a time window.
  Report disk size, number of objects, number of
  subdirectories.
}
\examples{
\dontrun{
## Find all objects stored in the directory tree starting
## at subdirectory specified by mantaSetwd(),
## return full Manta path to each object:
mantaFind()

## Find all objects ending in .jpg or .JPG
## in your Manta ~~/public directory and any child sub directories,
## (but not grandchildren), show a UNIX-like result sorted by file size:
mantaFind("~~/public", l = 'l', items = 'o', grepfor = "[.]jpg",
level = 2, ignore.case = TRUE, sortby = 'size')

## Download all objects in current Manta directory, non recursive find:
mantaGet(mantaFind(level = 1))

## Plot a histogram of all file sizes in your Manta  ~~/stor directory tree.
hist(mantaFind("~~/stor", l = 'sizes'))
}
}
\seealso{
  \code{\link{mantaLs}}, \code{\link{mantaLs.paths}},
  \code{\link{mantaLs.l}}, \code{\link{mantaLs.n}},
  \code{\link{mantaLs.du}}, \code{\link{mantaLs.url}}

  Other mantaFind: \code{\link{mantaFind.du}},
  \code{\link{mantaFind.l}}, \code{\link{mantaFind.n}},
  \code{\link{mantaFind.sizepath}},
  \code{\link{mantaFind.sizes}},
  \code{\link{mantaFind.url}}
}

