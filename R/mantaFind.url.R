# Roxygen Comments mantaFind.url
#' Recursive find tool for retrieving matching objects/subdirs from Manta hierarchy.
#' Output is URL format listing with full pathnames.
#'
#' Public HTTPS access is to objects in 
#' ~~/public subdirectory only.  ~~/stor objects are not accessible.
#' Provides searching for object or directory names with regular expressions (using R grep).
#' Sorting listings by filename, time, or size.
#' Can report disk size, number of objects, number of subdirectories.
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
