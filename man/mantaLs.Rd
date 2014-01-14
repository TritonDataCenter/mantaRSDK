\name{mantaLs}
\alias{mantaLs}
\title{Lists, searches, filters, sorts and formats Manta directory listings.}
\usage{
  mantaLs(mantapath, grepfor, json, l = "names",
    items = "a", sortby = "none", decreasing = FALSE,
    ignore.case = FALSE, perl = FALSE, verbose = FALSE,
    internal = FALSE)
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

  \item{l}{character optional.\cr Specifies listing output
  format by \code{'names', 'l', 'paths', 'URL', 'n', 'du',
  'R', 'Rraw', 'URL', 'json'.}.\cr \code{'names'} returns
  object/directory names.\cr \code{'l'} is a long unix ls
  -o style of directory listing.\cr \code{'paths'} is a
  listing of full Manta object pathnames.\cr \code{'n'} is
  the number of entries in the directory only.\cr
  \code{'du'} is the number of bytes used by objects (not
  counting redundancy levels!).\cr \code{'R'} is normalized
  R structures from JSON with size = 0 for directories,
  \code{mtime} in R time format.\cr \code{'URL'} is the
  browser format URL for objects, applies to
  \code{~~/public} objects only.\cr \code{'Rraw'} is R
  struct unparsed, unsorted, unnormalized, can convert back
  to json with \code{toJSON}.\cr \code{'json'} is exactly
  what the server replies - sorting/filtering are not
  applied.\cr}

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

  \item{internal}{logical, Internal use by mantaFind.}
}
\description{
  Used for getting disk size, number of objects, number of
  subdirectories. Searching for filenames with regular
  expressions (using R grep). Sorting listings by filename,
  time, or size.
}
\examples{
\dontrun{
## List names of  all objects stored in the directory
## specified by mantaSetwd(),
mantaLs()

## List all objects ending in .jpg or .JPG
## in your Manta ~~/public/images directory,
Show a UNIX-like result sorted by file size:
mantaLs("~~/public/images", l = 'l', items = 'o', grepfor = "[.]jpg",
ignore.case = TRUE, sortby = 'size')

## Download all objects in current Manta directory, non recursive find:
mantaGet(mantaLs.paths(items = 'o'))

}
}
\seealso{
  \code{\link{mantaFind}}

  Other mantaLs: \code{\link{mantaLs.du}},
  \code{\link{mantaLs.l}}, \code{\link{mantaLs.n}},
  \code{\link{mantaLs.paths}}, \code{\link{mantaLs.url}}
}
\keyword{Manta,}
\keyword{manta}

