\name{mantaFind}
\alias{mantaFind}
\title{Recursive find tool for retrieving matching objects/subdirs from Manta hierarchy}
\usage{
  mantaFind(mantapath, grepfor, entries, l = "paths",
    items = "o", sortby = "none", findroot = TRUE,
    decreasing = FALSE, ignore.case = FALSE, perl = FALSE,
    verbose = FALSE, info = TRUE)
}
\arguments{
  \item{mantapath}{string, required. Object/subdir in
  current subdirectory or full Manta path to stored object
  or subdirectory}

  \item{grepfor}{string optional. Regular expression passed
  to R grep for name search}

  \item{entries}{saved mantaFind R data, optional. For
  reprocessing retrieved tree with this funcion}

  \item{l}{string optional. Specifies listing output format
  by 'paths', 'n', 'du', 'R' 'paths' is a listing of full
  Manta object pathnames needed for mantaJobs 'l' is a
  Unix-y listing style with full pathnames 'n' is the
  number of entries found 'du' is the number of bytes used
  by objects (not counting redundancy levels!). 'R' is the
  R object collected by find with mtime parsed, full path
  names mantaFind(l='R') -> tree saves the directory tree
  for rerocessing with mantaFind(mantapath, entries = tree,
  ...)}

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

  \item{info}{logical, optional. Print status message about
  child path progress.}

  \item{findroot}{logical, internal. Indicates nested
  calls, not to be used.}
}
\description{
  Used for getting disk size, number of objects, number of
  subdirectories. Searching for object or directory names
  with regular expressions (using R grep). Sorting listings
  by filename, time, or size.
}

