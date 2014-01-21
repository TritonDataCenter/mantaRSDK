# Roxygen Comments mantaLs.url
#' Lists, searches, filters, sorts and formats Manta directory listings  
#' Output is URL path to \code{~~/public} Manta objects in the 
#' specified subdirectory 
#'
#' Used for getting URLs for links to object in Manta \code{~~/public} area.
#' Searching for filenames with regular expressions (using R \code{grep}). 
#' Sorting listings by filename, time, or size.
#'
#' @inheritParams mantaLs  
#'
#' @seealso \code{\link{mantaFind.url}}
#'
#' @family mantaLs  
#'
#' @keywords Manta
#'
#' @export
mantaLs.url <-
function(mantapath, grepfor, json,  items = 'o', sortby = 'none', 
          decreasing = FALSE, ignore.case = FALSE, perl = FALSE, verbose = FALSE) {

  return(mantaLs(mantapath, l='URL', grepfor = grepfor, json = json, items = items,
                 sortby = sortby, decreasing = decreasing, ignore.case = ignore.case,
                 perl = perl, verbose = verbose))
}


