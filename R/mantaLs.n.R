# Roxygen Comments mantaLs.n
#' Returns number of files matched in Manta directory listing.
#'  
#'
#' Used for getting number of objects/subdir in a directory. Default uses
#' current Manta directory. 
#'
#' @inheritParams mantaLs  
#'
#' @seealso \code{\link{mantaFind.n}}
#'
#' @family mantaLs  
#'
#' @keywords Manta
#'
#' @export
mantaLs.n <-
function(mantapath, grepfor, json,  items = 'a', 
          ignore.case = FALSE, perl = FALSE, verbose = FALSE) {

  return(mantaLs(mantapath, l='n', grepfor = grepfor, json = json, items = items,
                 sortby = 'none', decreasing = FALSE, ignore.case = ignore.case,
                 perl = perl, verbose = verbose))
}


