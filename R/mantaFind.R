# Roxygen Comments mantaFind
#' Recursive find tool for retrieving matching objects/subdirs from Manta hierarchy
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
#' @param l string optional. Specifies listing output format by 'paths',
#' 'n', 'du', 'R'
#' 'paths' is a listing of full Manta object pathnames needed for mantaJobs
#' 'l' is a Unix-y listing style with full pathnames 
#' 'n' is the number of entries found
#' 'du' is the number of bytes used by objects (not counting redundancy levels!).
#' 'R' is the R object collected by find with mtime parsed, full path names
#'  mantaFind(l='R') -> tree saves the directory tree for rerocessing with
#'  mantaFind(mantapath, entries = tree, ...) Parameters grepfor and items are 
#'  ignored on reprocessed trees.   
#'
#' @param items string optional. 'a' for all, 'd' for directory, 'o' for object.
#  Ignored for reprocessed trees
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
#' @param info logical, optional. Print status  message about child path progress.
#'
#' @param findroot logical, internal. Indicates nested calls, not to be used.
#'
#' @export
mantaFind <-
function(mantapath, grepfor, entries, l = 'paths', items = 'o', sortby = 'none', findroot = TRUE,
          decreasing = FALSE, ignore.case = FALSE, perl = FALSE, verbose = FALSE, info = TRUE) {

  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

  if (missing(mantapath)) {
      mantapath <- mantaGetwd()
  }

  path_enc <- mantaPath(mantapath)
  

  validl <- c('paths', 'l', 'n', 'du', 'R')
  validsortby <- c('none', 'name', 'time', 'size')
  validitems <- c('a', 'd', 'o')

  if (is.na(match(l,validl))) {
    stop("mantaRSDK:mantaFind Invalid l='",l,"' argument, try help(mantaFind)\n")
  }
  if (is.na(match(sortby,validsortby))) {
    stop("mantaRSDK:mantaFind Invalid sortby='",sortby,"' argument, try help(mantaFind)\n")
  }
  if (is.na(match(items,validitems))) {
    stop("mantaRSDK:mantaFind Invalid items='",items,"' argument, try help(mantaFind)\n")
  }

  if ((findroot == TRUE) && (info == TRUE)) {
    cat("mantaFind start at: ",path_enc,"\n")
  }

  if (missing(entries)) {

    # get the raw directory listing in json format from Manta
    json <- mantaLs(mantapath=curlUnescape(path_enc), l ='json')

    if (length(json) == 1) {
      if (json[1] == "") return("")
    }

    # extract the subdirectories as paths for recursive descent
    subdirs <- mantaLs(mantapath = curlUnescape(path_enc), json=json, l = 'paths', items = 'd')
    result <- list()

    if (length(subdirs[subdirs != ""]) != 0) {
    # there are subdirectories here
    # recurse through the subdirectories
    # no sorting, return R objects that match filters
      for (i in 1:length(subdirs)) {
       if (info == TRUE) {
         cat("Fetching child path: ",subdirs[i],"\n")
       }
       result <- c(result, mantaFind(subdirs[i], grepfor = grepfor, l = 'R', items = items, sortby = 'none',
                    findroot = FALSE, verbose = verbose))
       result <- result[result != ""]  #remove null results, it is ok if this is an empty list.
      }
    }
  
    # Child directory entries are now gathered
    # prepare the current directory R entries, grep/items filtered
    # no sorting yet - this is done with saved json fetched above
    cur_results <- mantaLs(mantapath=curlUnescape(path_enc), json=json, l = 'R', items = items, grepfor = grepfor,
                         ignore.case = ignore.case, perl = perl, sortby = 'none', verbose = verbose)
  

    # Prepend path callback to replace name with full pathname
    prependpath <- function(line) {
     if (!is.na(charmatch("name",names(line)))) {
       line$name <- (paste(curlUnescape(path_enc), "/", line$name, sep=""))
      }
      if (!is.na(charmatch("id",names(line)))) {
       line$id <- (paste(curlUnescape(path_enc), "/", line$id, sep=""))
      }
      return(line)
    }
 
    # Are there any results in the current path 
    if (length(cur_results[cur_results != ""]) != 0) {  

      # Then prepend the path to all the results to mark their origin subdir
      cur_results <- lapply(cur_results,prependpath)
    }

    # Attach to the child results, these are all R stuctures so far
    # and can be  empty
    entries <- c(result, cur_results)

    if (findroot == FALSE) {
      # Return the found R structured entries with children
      # and all the embedded pathnames 
      # to the parent mantaFind call
      return(entries)
    }

    # This is the top level call to mantaFind
    # Here - all the children are retrieved
    # Do any requested sorting on the R results
    # and return in requested output format

  } else {
    # user-supplied tree made from previous mantaFind call
    if (info == TRUE) {
    cat("Using previously retrieved mantaFind directory\n")
    }   
  }

  # Cleanup 
  if (length(entries) == 0) return("")
  entries <- entries[!sapply(entries, is.null)]
  entries <- entries[entries != ""]
  if (length(entries) == 0) return("")
    
  # set the output format, for numbers, return those
  switch(l,
      R={
        lines <- entries
      },
      paths={ 
      # note we are using the liststyle callback here
      # as the R entries have full paths now embeded in the
      # name field        
        lines <- lapply(entries, mantaliststyle)
      },
      l={
        lines <- lapply(entries, mantaunixstyle)
      },
      n={
      # return number of entries
        return(length(entries))
      },
      du={
      # return disk size utilized of entries, in bytes
      # not factoring in number of reduncant copies
        return(sum(unlist(lapply(entries, mantadusize))))
      }
  )

  ### Sorted/filtered R entries or formatted lines
  switch(sortby,
      none={
      },
      name={
        names <- lapply(entries, mantagetnames)
        lines <- lines[order(unlist(names), decreasing = decreasing)]
      },
      size={
        sizes <- lapply(entries, mantagetsize)
        lines <- lines[order(unlist(sizes), decreasing = decreasing)]
      },
      time={
        times <- lapply(entries, mantagettime)
        times <- do.call(c,times) # Keeps the  R time structure for sorting...
        lines <- lines[order(times, decreasing = decreasing)]
      }
  )

  # Return results in l format 'paths', 'n', 'du', 'R'
  if (l == "R") {  # don't flatten the R stuctures
      return(lines)
  } else { # flatten the lines
      return(do.call(c,lines))
  }
  
}
