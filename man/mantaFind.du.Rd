\name{mantaFind.du}
\alias{mantaFind.du}
\title{Recursive find tool for retrieving matching objects/subdirs from Manta hierarchy
Output is disk utilized in bytes NOT counting redundancy levels}
\usage{
  mantaFind.du(mantapath, grepfor, entries, items = "o",
    level = 0, starttime, endtime, ignore.case = FALSE,
    perl = FALSE, verbose = FALSE, info = TRUE)
}
\arguments{
  \item{mantapath}{string, required. Object/subdir in
  current subdirectory or full Manta path to stored object
  or subdirectory}

  \item{grepfor}{string optional. Regular expression passed
  to R grep for name search. USE "[.]txt" to match
  extensions, not ".txt"}

  \item{entries}{saved mantaFind R data, optional. For
  reprocessing/reformatting retrieved R tree information
  saved with mantaFind(l='R')->tree}

  \item{items}{string optional. 'a' for all, 'd' for
  directory, 'o' for object.}

  \item{level}{integer optional. Maximum number of
  subdirectory child levels to visit, in other words, the
  depth of the hierarchical directory search. If level <=
  0, search depth is unrestricted. Level parameter is
  ignored on reprocessed search trees.}

  \item{starttime}{POSIXlt time, optional. Start time for
  time-bounded find. When used without endtime, endtime is
  set to current UTC time.}

  \item{endtime}{POSIXlt time, optional. End time for
  time-bounded find. When used without starttime, starttime
  is set to start of Manta service}

  \item{ignore.case}{logical, optional. Argument passed to
  R grep for searching.}

  \item{perl}{logical, optional. Argument passed to R grep
  for searching.}

  \item{verbose}{logical, optional. Verbose HTTP data
  output on Unix.}

  \item{info}{logical, optional. Print status message about
  child path progress.}
}
\description{
  Searching for object or directory names with regular
  expressions (using R grep). Sorting listings by filename,
  time, or size. Can report disk size, number of objects,
  number of subdirectories.
}

