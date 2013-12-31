# Roxygen Comments mantaLs.l
#' Lists, searches, filters, sorts and formats Manta directory listings  
#' Output is long ls -o unix style of directory listing
#'
#' Used for getting disk size, number of objects, number of subdirectories.
#' Searching for filenames with regular expressions (using R grep). 
#' Sorting listings by filename, time, or size
#'
#' @param mantapath string, required. Object/subdir in current subdirectory
#' or full Manta path to stored object or subdirectory
#'
#' @param grepfor string optional. Regular expression passed to R grep for name search 
#' USE "[.]txt" to match extensions, not ".txt"
#' 
#' @param items string optional. 'a' for all, 'd' for directory, 'o' for object. 
#' 
#' @param sortby string, optional. Specify 'none', 'name', 'time', or 'size'.
#' 
#' @param decreasing logical, optional. Argument passed to R order for sorting. 
#' 
#' @param ignore.case logical, optional. Argument passed to R grep for searching.
#' 
#' @param perl logical, optional. Argument passed to R grep for searching. 
#' 
#' @param verbose logical, optional. Verbose HTTP data output on Unix.
#'
#' @param json, optional. Input saved JSON data from mantaLs(format='json') 
#' used for reprocessing previously retrieved listings with specified
#' mantapath if you wish to recover true 'paths'.
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


