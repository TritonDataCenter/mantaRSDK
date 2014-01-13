# Roxygen Comments mantaFind.sizes
#' Recursive find tool for retrieving matching objects/subdirs from Manta hierarchy.
#' Output is vector of file sizes in bytes, no file or path names.
#'
#' Searching for object or directory names with regular expressions (using R grep).
#'
#' @inheritParams mantaFind
#'
#' @family mantaFind
#'
#' @export
mantaFind.sizes <-
function(mantapath, grepfor, entries, items = 'o', level = 0, sortby = 'none', starttime, endtime,
          decreasing = FALSE, ignore.case = FALSE, perl = FALSE, verbose = FALSE, info = TRUE) {

   return(mantaFind(mantapath = mantapath, l='sizes', grepfor = grepfor, entries = entries,
                    items = items, level = level, starttime = starttime, endtime = endtime, sortby = sortby, 
                    findroot = 1, decreasing = decreasing, ignore.case = ignore.case, perl = perl,
                    verbose = verbose, info = info))   
}
