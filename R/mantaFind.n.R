# Roxygen Comments mantaFind.n
#' Recursive find tool for retrieving matching objects/subdirs from Manta hierarchy.
#' Output is number of found entries.
#'
#' Searching for object or directory names with regular expressions (using R grep).
#' Sorting listings by filename, time, or size.
#' Can report disk size, number of objects, number of subdirectories.
#'
#' @inheritParams mantaFind
#'
#' @family mantaFind
#'
#' @seealso \code{\link{mantaLs.n}}
#'
#' @export
mantaFind.n <-
function(mantapath, grepfor, entries, items = 'o', level = 0, starttime, endtime,
          ignore.case = FALSE, perl = FALSE, verbose = FALSE, info = TRUE) {

   return(mantaFind(mantapath = mantapath, l='n', grepfor = grepfor, entries = entries,
                    items = items, level = level, starttime = starttime, endtime = endtime, 
                    sortby = 'none', findroot = 1, 
                    decreasing = FALSE, ignore.case = ignore.case, perl = perl,
                    verbose = verbose, info = info))   
}
