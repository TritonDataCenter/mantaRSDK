# Roxygen Comments mantaLs.paths
#' Lists, searches, filters, sorts and formats Manta directory listings  
#' Output is full Manta pathnames of directory listing.
#'
#' Searches for filenames with regular expressions (using R \code{grep}). 
#' Sorts listings by filename, time, or size. Use this dotted form of 
#' \code{mantaLs} 
#' for passing \code{mantapath} parameters into vectorized functions like
#' \code{mantaJob.launch}, \code{mantaGet}, \code{mantaSnapln}, etc.
#'
#' @inheritParams mantaLs  
#'
#' @seealso \code{\link{mantaFind}}
#'
#' @family mantaLs  
#'
#' @keywords Manta
#'
#' @export
mantaLs.paths <-
function(mantapath, grepfor, json,  items = 'a', sortby = 'none', 
          decreasing = FALSE, ignore.case = FALSE, perl = FALSE, verbose = FALSE) {

  return(mantaLs(mantapath, l='paths', grepfor = grepfor, json = json, items = items,
                 sortby = sortby, decreasing = decreasing, ignore.case = ignore.case,
                 perl = perl, verbose = verbose))
}


