\name{mantaLs.url}
\alias{mantaLs.url}
\title{mantaLs.url lists, searches, filters, sorts and formats directory listings
Output is long ls -o unix style of directory listing}
\usage{
  mantaLs.url(mantapath, grepfor, json, items = "a",
    sortby = "none", decreasing = FALSE,
    ignore.case = FALSE, perl = FALSE, verbose = FALSE)
}
\arguments{
  \item{mantapath}{string, required. Object/subdir in
  current subdirectory or full Manta path to stored object
  or subdirectory}

  \item{grepfor}{string optional. Regular expression passed
  to R grep for name search USE "[.]txt" to match
  extensions, not ".txt"}

  \item{items}{string optional. 'a' for all, 'd' for
  directory, 'o' for object.}

  \item{sortby}{string, optional. Specify 'none', 'name',
  'time', or 'size'.}

  \item{decreasing}{logical, optional. Argument passed to R
  order for sorting.}

  \item{ignore.case}{logical, optional. Argument passed to
  R grep for searching.}

  \item{perl}{logical, optional. Argument passed to R grep
  for searching.}

  \item{verbose}{logical, optional. Verbose HTTP data
  output on Unix.}

  \item{json,}{optional. Input saved JSON data from
  mantaLs(format='json') used for reprocessing previously
  retrieved listings with specified mantapath if you wish
  to recover true 'paths'.}
}
\description{
  Used for getting disk size, number of objects, number of
  subdirectories. Searching for filenames with regular
  expressions (using R grep). Sorting listings by filename,
  time, or size
}
\keyword{Manta,}
\keyword{manta}

