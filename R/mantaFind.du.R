# Roxygen Comments mantaFind.du
#' Recursive find tool for retrieving matching objects/subdirs from Manta hierarchy.
#' Output is disk utilized in bytes NOT counting redundancy levels.
#'
#' Search for object or directory names with regular expressions (using R grep).
#' Reports disk size.
#'
#' @inheritParams mantaFind
#'
#' @seealso \code{\link{mantaLs.du}}
#'
#' @family mantaFind
#'
#' @export
mantaFind.du <-
function(mantapath, grepfor, entries, items = 'o', level = 0, starttime, endtime,
          ignore.case = FALSE, perl = FALSE, verbose = FALSE, info = TRUE) {

   return(mantaFind(mantapath = mantapath, l='du', grepfor = grepfor, entries = entries,
                    items = items, level = level, starttime = starttime, endtime = endtime,
                    sortby = 'none', findroot = 1, decreasing = FALSE, ignore.case = ignore.case, perl = perl,
                    verbose = verbose, info = info))   
}
