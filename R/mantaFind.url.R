# Roxygen Comments mantaFind.url
#' Recursive find tool for retrieving matching objects/subdirs from Manta hierarchy.
#' Output is URL format listing with full pathnames.
#'
#' Public HTTPS access is to objects in 
#' ~~/public subdirectory only.  ~~/stor objects are not accessible.
#' Search for object or directory names with regular expressions (using R grep).
#' Sort listings by filename, time, or size.
#'
#' @inheritParams mantaFind
#'
#' @family mantaFind
#'
#' @seealso \code{\link{mantaLs.url}}
#'
#' @export
mantaFind.url <-
function(mantapath, grepfor, entries, items = 'o', level = 0, sortby = 'none', starttime, endtime,
          decreasing = FALSE, ignore.case = FALSE, perl = FALSE, verbose = FALSE, info = TRUE) {

   return(mantaFind(mantapath = mantapath, l='URL', grepfor = grepfor, entries = entries,
                    items = items, level = level, sortby = sortby, starttime = starttime, endtime = endtime, 
                    findroot = 1,  decreasing = decreasing, ignore.case = ignore.case, perl = perl,
                    verbose = verbose, info = info))   
}
