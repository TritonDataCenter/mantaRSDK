\name{mantaFind.sizes}
\alias{mantaFind.sizes}
\title{Recursive find tool for retrieving matching objects/subdirs from Manta hierarchy
Output is vector of file sizes in bytes, no file or path names}
\usage{
  mantaFind.sizes(mantapath, grepfor, entries, items = "o",
    level = 0, sortby = "none", decreasing = FALSE,
    ignore.case = FALSE, perl = FALSE, verbose = FALSE,
    info = TRUE)
}
\arguments{
  \item{mantapath}{string, required. Object/subdir in
  current subdirectory or full Manta path to stored object
  or subdirectory}

  \item{grepfor}{string optional. Regular expression passed
  to R grep for name search. Ignored for reprocessed trees}

  \item{entries}{saved mantaFind R data, optional. For
  reprocessing/reformatting retrieved R tree information
  saved with mantaFind(l='R')->tree}

  \item{items}{string optional. 'a' for all, 'd' for
  directory, 'o' for object. Ignored for reprocessed trees}

  \item{level}{integer optional. Maximum number of
  subdirectory child levels to visit, in other words, the
  depth of the hierarchical directory search. If level <=
  0, search depth is unrestricted. Level parameter is
  ignored on reprocessed search trees.}

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

  \item{info}{logical, optional. Console status message
  about child path progress.}
}
\description{
  Searching for object or directory names with regular
  expressions (using R grep).
}

