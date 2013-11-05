\name{mantaLs}
\alias{mantaLs}
\title{mantaLs lists, searches, filters, sorts and formats directory listings}
\usage{
  mantaLs(mantapath, grepfor, json, l = "names",
    items = "a", sortby = "none", decreasing = FALSE,
    ignore.case = FALSE, perl = FALSE, verbose = FALSE)
}
\arguments{
  \item{mantapath}{string, required. Object/subdir in
  current subdirectory or full Manta path to stored object
  or subdirectory}

  \item{grepfor}{string optional. Regular expression passed
  to R grep for name search}

  \item{l}{string optional. Specifies listing output format
  by 'names', 'l', 'paths', 'n', 'du', 'R', 'Rraw', 'json'.
  'names' returns object/directory names. 'l' is a long ls
  -o style of directory listing. 'paths' is a listing of
  full Manta object pathnames. 'n' is the number of entries
  in the directory only. 'du' is the number of bytes used
  by objects (not counting redundancy levels!). 'R' is
  normalized R structures from JSON with size = 0 for
  directories, mtime in R time format. 'Rraw' is R struct
  unparsed, unsorted, unnormalized, can convert back to
  json with toJSON. 'json' is exactly what the server
  replies - sorting/filtering are not applied.}

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

