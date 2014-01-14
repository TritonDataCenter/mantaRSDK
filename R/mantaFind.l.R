# Roxygen Comments mantaFind.l
#' Recursive find tool for retrieving matching objects/subdirs from Manta hierarchy.
#' Output is long \code{ls -o} unix-y style listing with full pathnames.

#' Search for object or directory names with regular expressions (using R grep).
#' Sort listings by filename, time, or size.
#'
#' @inheritParams mantaFind
#'
#' @family mantaFind
#'
#' @export
mantaFind.l <-
function(mantapath, grepfor, entries, items = 'o', level = 0, sortby = 'none', starttime, endtime,
          decreasing = FALSE, ignore.case = FALSE, perl = FALSE, verbose = FALSE, info = TRUE) {

   return(mantaFind(mantapath = mantapath, l='l', grepfor = grepfor, entries = entries,
                    items = items, level = level, sortby = sortby, starttime = starttime, endtime = endtime,
                    findroot = 1, decreasing = decreasing, ignore.case = ignore.case, perl = perl,
                    verbose = verbose, info = info))   
}
