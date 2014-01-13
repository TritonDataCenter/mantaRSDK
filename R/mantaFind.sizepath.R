# Roxygen Comments mantaFind.sizepath
#' Recursive find tool for retrieving matching objects/subdirs from Manta hierarchy.
#' Output is R list of size and full manta pathnames.
#'
#' Searching for object or directory names with regular expressions (using R grep).
#' Sorting listings by filename, time, or size.
#' Can report disk size, number of objects, number of subdirectories.
#'
#' @inheritParams mantaFind
#'
#' @family mantaFind
#'
#' @export
mantaFind.sizepath <-
function(mantapath, grepfor, entries, items = 'o', level = 0, sortby = 'none', starttime, endtime,
          decreasing = FALSE, ignore.case = FALSE, perl = FALSE, verbose = FALSE, info = TRUE) {

   return(mantaFind(mantapath = mantapath, l='size_path', grepfor = grepfor, entries = entries,
                    items = items, level = level, starttime = starttime, endtime = endtime, sortby = sortby, 
                    findroot = 1,decreasing = decreasing, ignore.case = ignore.case, perl = perl,
                    verbose = verbose, info = info))   
}
