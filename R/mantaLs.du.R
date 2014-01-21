# Roxygen Comments mantaLs.du
#' Returns disk used in bytes of directory listings, NOT counting
#' redundancy levels  
#'
#' Used for getting number of bytes occupied by objects matching directory query.
#'
#' @inheritParams mantaLs
#'
#' @seealso \code{\link{mantaFind.du}}
#'
#' @family mantaLs
#'
#'
#' @keywords Manta
#'
#' @export
mantaLs.du <-
function(mantapath, grepfor, json,  items = 'a',
          ignore.case = FALSE, perl = FALSE, verbose = FALSE) {

  return(mantaLs(mantapath, l='du', grepfor = grepfor, json = json, items = items,
                 sortby = 'none', decreasing = FALSE, ignore.case = ignore.case,
                 perl = perl, verbose = verbose))
}


