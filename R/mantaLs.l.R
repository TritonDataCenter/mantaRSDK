# Roxygen Comments mantaLs.l
#' Lists, searches, filters, sorts and formats Manta directory listings  
#' Output is long \code{ls -o} unix style of directory listing.
#'
#' Used for listing Manta subdirectory. Uses current working Manta directory
#' or one supplied in \code{mantapath}.
#' Searches for filenames with regular expressions (using R \code{grep}). 
#' Sorts listings by filename, time, or size.
#'
#' @inheritParams mantaLs  
#'
#' @seealso \code{\link{mantaFind.l}}
#'
#' @family mantaLs  
#'
#' @keywords Manta, manta
#'
#' @export
mantaLs.l <-
function(mantapath, grepfor, json,  items = 'a', sortby = 'none', 
          decreasing = FALSE, ignore.case = FALSE, perl = FALSE, verbose = FALSE) {

  return(mantaLs(mantapath, l='l', grepfor = grepfor, json = json, items = items,
                 sortby = sortby, decreasing = decreasing, ignore.case = ignore.case,
                 perl = perl, verbose = verbose))
}


