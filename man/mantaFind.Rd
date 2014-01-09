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
  \item{mantapath}{string, required. Object/subdir in
  current subdirectory or full Manta path to stored object
  or subdirectory}

  \item{grepfor}{string optional. Regular expression passed
  to R grep for name search. Ignored for reprocessed trees.
  Uses R regexps, e.g. USE "[.]txt", NOT ".txt" to match
  filename extensions.}

  \item{entries}{saved mantaFind R data, optional. For
  reprocessing/reformatting retrieved R tree information
  saved with mantaFind(l='R')->tree}

  \item{l}{string optional. Specifies listing output format
  by 'paths', 'n', 'du', 'R' 'paths' is a listing of full
  Manta object pathnames needed for mantaJobs 'l' is a
  Unix-y listing style with full pathnames 'sizes' is a
  listing of sizes in bytes, no pathnames 'size_path' is a
  listing of size [space] path 'URL' is a listing of the
  URLs (access applies to objects in ~~/public/ only) 'n'
  is the number of entries found 'du' is the number of
  bytes used by objects (not counting redundancy levels!).
  'R' is the R object collected by find with mtime parsed,
  full path names mantaFind(l='R') -> tree saves the
  directory tree for rerocessing with mantaFind(mantapath,
  entries = tree, ...)}

  \item{items}{string optional. 'a' for all, 'd' for
  directory, 'o' for object.}

  \item{level}{integer optional. Maximum number of
  subdirectory child levels to visit, in other words, the
  depth of the hierarchical directory search. If level <=
  0, search depth is unrestricted. Level parameter is
  ignored on reprocessed search trees.}

  \item{sortby}{string, optional. Specify 'none', 'name',
  'time', or 'size'. Sorting selection is independent of
  time-bounded find.}

  \item{starttime}{POSIXlt time, optional. Start time for
  time-bounded find. When used without endtime, endtime is
  set to current UTC time.}

  \item{endtime}{POSIXlt time, optional. End time for
  time-bounded find. When used without starttime, starttime
  is set to start of Manta service}

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

  \item{findroot}{integer, internal. Indicates nested
  calls, not to be used.}
}
\description{
  Searching for object or directory names with regular
  expressions (using R grep). Sorting listings by filename,
  time, or size. Can report entries within a time window.
  Can report disk size, number of objects, number of
  subdirectories.
}

