\name{mantaLs.n}
\alias{mantaLs.n}
\title{mantaLs.n Returns number of files matched in directory listing}
\usage{
  mantaLs.n(mantapath, grepfor, json, items = "a",
    ignore.case = FALSE, perl = FALSE, verbose = FALSE)
}
\arguments{
  \item{mantapath}{string, required. Object/subdir in
  current subdirectory or full Manta path to stored object
  or subdirectory}

  \item{grepfor}{string optional. Regular expression passed
  to R grep for name search}

  \item{items}{string optional. 'a' for all, 'd' for
  directory, 'o' for object.}

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
  Used for getting number of objects/subdir in a directory.
  Default uses current Manta directory
}
\keyword{Manta,}
\keyword{manta}

