# Roxygen Comments mantaFind.sizepath
#' Recursive find tool for retrieving matching objects/subdirs from Manta hierarchy
#' Output is R list of size and full manta pathnames
#'
#' Searching for object or directory names with regular expressions (using R grep).
#' Sorting listings by filename, time, or size.
#' Can report disk size, number of objects, number of subdirectories.
#'
#' @param mantapath string, required. Object/subdir in current subdirectory
#' or full Manta path to stored object or subdirectory
#'
#' @param grepfor string optional. Regular expression passed to R grep for name search.
#' Ignored for reprocessed trees
#'
#' @param entries saved mantaFind R data, optional. For reprocessing/reformatting 
#' retrieved R tree information saved with  mantaFind(l='R')->tree
#'
#' @param items string optional. 'a' for all, 'd' for directory, 'o' for object.
#' Ignored for reprocessed trees
#'
#' @param level integer optional. Maximum number of subdirectory child levels 
#' to visit, in other words, the depth of the hierarchical directory search. If level
#' <= 0, search depth is unrestricted. Level parameter is ignored on reprocessed 
#' search trees.
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
#' @param info logical, optional. Console status message about child path progress.
#'
#' @export
mantaFind.sizepath <-
function(mantapath, grepfor, entries, items = 'o', level = 0, sortby = 'none',
          decreasing = FALSE, ignore.case = FALSE, perl = FALSE, verbose = FALSE, info = TRUE) {

   return(mantaFind(mantapath = mantapath, l='size_path', grepfor = grepfor, entries = entries,
                    items = items, level = level, sortby = sortby, findroot = 1, 
                    decreasing = decreasing, ignore.case = ignore.case, perl = perl,
                    verbose = verbose, info = info))   
}
