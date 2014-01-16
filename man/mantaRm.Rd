\name{mantaRm}
\alias{mantaRm}
\title{Removes specified Manta object, optionally recursive, not vectorized.}
\usage{
  mantaRm(mantapath, r = FALSE, info = TRUE)
}
\arguments{
  \item{mantapath}{character, required. Not vectorized.}

  \item{r,}{logical optional. Set \code{TRUE} for recursive
  delete of all objects within all child subdirectories,
  and the directories.}

  \item{info}{logical. Set FALSE to suppress console
  messages.}
}
\value{
  \code{TRUE} or \code{FALSE} depending on success of
  remove.
}
\description{
  Removes object. Specify absolute (e.g.
  \code{~~/stor/myobject.txt} ) or relative path from the
  current Manta directory. Supports recursive removal of
  child contents objects and directories with \code{r =
  TRUE}. You can use \code{mantaFind} to prepare a list of
  absolute Manta pathnames to objects with detailed
  searching and filtering specifications and then use
  \code{lapply(pathnamelist, mantaRm)} to delete the items.
}
\examples{
\dontrun{
data <- runif(100)
mantaDump("data")
mantaCat("dumpdata.R")
mantaRm("dumpdata.R")

## Make a hierarchical directory set, for removal:
mantaGetwd() -> tempdir
mantaMkdir("~~/stor/a_test/b_test/c_test", p = TRUE)
mantaSetwd("~~/stor/a_test/b_test/c_test")
mantaMkdir("d_test")   # Relative path
mantaDump("data_c")
mantaSetwd("..")
mantaDump("data_b")
mantaSetwd("..")
mantaDump("data_a")
mantaLs.l()
mantaFind()

## Recursive Rm Subdirectory Contents
mantaSetwd.stor()
mantaRm("~~/stor/a_test", r = TRUE)
mantaSetwd("~~/stor/a_test")
mantaLs.l()
mantaFind()
mantaSetwd.stor()
mantaRmdir("~~/stor/a_test")
mantaLs.l("~~/stor")
mantaSetwd(tempdir)
}
}
\seealso{
  Other Directory: \code{\link{mantaGetwd}},
  \code{\link{mantaMkdir}}, \code{\link{mantaRmdir}},
  \code{\link{mantaSetwd}},
  \code{\link{mantaSetwd.public}},
  \code{\link{mantaSetwd.reports}},
  \code{\link{mantaSetwd.stor}}, \code{\link{mantaSnapln}}
}
\keyword{Manta,}
\keyword{manta}

