# TODO fix wrong initial directory error - unwrap first call to mantaLs()
# Roxygen Comments mantaFind
#' Recursive find tool for retrieving matching objects/subdirs from Manta hierarchy.
#'
#' Searching for object or directory names with regular expressions (using R grep).
#' Sorting listings by filename, time, or size. Can report entries within a time
#' window. Can report disk size, number of objects, number of subdirectories.
#'
#' @param mantapath string, required. Object/subdir in current subdirectory
#' or full Manta path to stored object or subdirectory
#'
#' @param grepfor string optional. Regular expression passed to R grep for name search.
#' Ignored for reprocessed trees. Uses R regexps, e.g. USE "[.]txt", NOT ".txt" to 
#' match filename extensions.
#'
#' @param entries saved mantaFind R data, optional. For reprocessing/reformatting 
#' retrieved R tree information saved with  mantaFind(l='R')->tree
#'
#' @param l string optional. Specifies listing output format by 'paths',
#' 'n', 'du', 'R'
#' 'paths' is a listing of full Manta object pathnames needed for mantaJobs
#' 'l' is a Unix-y listing style with full pathnames 
#' 'sizes' is a listing of sizes in bytes, no pathnames
#' 'size_path' is a listing of size [space] path
#' 'URL' is a listing of the URLs (access applies to objects in ~~/public/ only)
#' 'n' is the number of entries found
#' 'du' is the number of bytes used by objects (not counting redundancy levels!).
#' 'R' is the R object collected by find with mtime parsed, full path names
#'  mantaFind(l='R') -> tree saves the directory tree for rerocessing with
#'  mantaFind(mantapath, entries = tree, ...) 
#'
#' @param items string optional. 'a' for all, 'd' for directory, 'o' for object.
#'
#' @param level integer optional. Maximum number of subdirectory child levels 
#' to visit, in other words, the depth of the hierarchical directory search. If level
#' <= 0, search depth is unrestricted. Level parameter is ignored on reprocessed 
#' search trees.
#'
#' @param sortby string, optional. Specify 'none', 'name', 'time', or 'size'.
#' Sorting selection is independent of time-bounded find.
#'
#' @param starttime POSIXlt time, optional. Start time for time-bounded find.
#' When used without endtime, endtime is set to current UTC time.
#'
#' @param endtime POSIXlt time, optional. End time for time-bounded find.
#' When used without starttime, starttime is set to start of Manta service
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
#' @param findroot integer, internal. Indicates nested calls, not to be used.
#'
#' @export
mantaFind <-
function(mantapath, grepfor, entries, l = 'paths', items = 'o', level = 0, sortby = 'none', starttime, endtime,
          decreasing = FALSE, ignore.case = FALSE, perl = FALSE, verbose = FALSE, info = TRUE, findroot = 1) {

  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

  if (missing(mantapath)) {
      mantapath <- mantaGetwd()
  }

  path_enc <- mantaPath(mantapath)
  

  validl <- c('paths', 'l', 'n', 'du', 'R', 'sizes', 'size_path', 'URL')
  validsortby <- c('none', 'name', 'time', 'size')
  validitems <- c('a', 'd', 'o')

  if (is.na(match(l,validl))) {
    stop("mantaFind Invalid l='",l,"' argument, try help(mantaFind)\n")
  }
  if (is.na(match(sortby,validsortby))) {
    stop("mantaFind Invalid sortby='",sortby,"' argument, try help(mantaFind)\n")
  }
  if (is.na(match(items,validitems))) {
    stop("mantaFind Invalid items='",items,"' argument, try help(mantaFind)\n")
  }

  if (l == 'URL') {
    # must begin with /$MANTA_USER/public
    lead <- paste("/", manta_globals$manta_user, "/public", sep="")
    if  (is.na(charmatch(lead, path_enc))) {
       stop("mantaFind Invalid Manta subdirectory for public URLs - must be in ", lead)
    }
  }


  timerange <- FALSE
  if ( (!missing(starttime)) && (!missing(endtime)) ) {
    # Time window - arg checking
    capture.output(str(starttime, give.head=FALSE, give.length=FALSE, no.list=TRUE)) -> starttime_str
    capture.output(str(endtime, give.head=FALSE, give.length=FALSE, no.list=TRUE)) -> endtime_str
    if ( (length(grep("POSIXlt", starttime_str)) == 0) ||
         (length(grep("POSIXlt", endtime_str)) == 0) ) {
      stop("mantaFind Invalid parameters\nstarttime and endtime values must be properly formed as R time variables using as.POSIXlt()\n")
    }
    if ((endtime - starttime) < 0) {
      temptime <- starttime
      starttime <- endtime
      endtime <- temptime
    }
    timerange <- TRUE
  }
  
  if ( (missing(starttime)) && (!missing(endtime)) ) {
    # time window up to 
    capture.output(str(endtime, give.head=FALSE, give.length=FALSE, no.list=TRUE)) -> endtime_str
    if ( (length(grep("POSIXlt", endtime_str)) == 0) ) {
      stop("mantaFind Invalid parameters\nendtime value must be properly formed as R time variables using as.POSIXlt()\n")
    }
    # Set the start time to the beginning of Manta Service
    starttime <- as.POSIXlt("2013-01-01 00:01", "UTC") # Before this date Manta did not exist
    if ((endtime - starttime) < 0) {
       stop("mantaFind Invalid parameters\n endtime specified is older than Manta itself, noting to find\n")
    }
    timerange <- TRUE
  }
  
  if ( (missing(endtime)) && (!missing(starttime)) ) {
    capture.output(str(starttime, give.head=FALSE, give.length=FALSE, no.list=TRUE)) -> starttime_str
    if ( (length(grep("POSIXlt", starttime_str)) == 0) ) {
      stop("mantaFind Invalid parameters\n starttime value must be properly formed as R time variables using as.POSIXlt()\n")
    } 
    # Set the end time to now.
    endtime <-  as.POSIXlt(Sys.time(), "UTC")
    if ((endtime - starttime) < 0) {
       stop("mantaFind Invalid parameters\n starttime specified is in the future, noting to find\n")
    }
    timerange <- TRUE
  }


  msg <- paste("mantaFind: start at ", mantapath ,sep="")
  bunyanLog.info(msg = msg)
  if ((findroot == 1) && (info == TRUE)) {
    cat(msg,"\n")
  }


  if (missing(entries)) {

    if (findroot == 1) {
      # error Setpoint at the start of call to mantaFind
      bunyanClearSetpoint()
      bunyanSetpoint()
      assign("find_list",list(),envir=manta_globals)
      assign("find_dir_count", 0, envir=manta_globals)
      countdown <- manta_globals$manta_defaults$connect_timeout # default is 5s
#  here try mantaLs and stop on wrong starting directory errors
#
#

    } else {
      countdown <- manta_globals$manta_defaults$receive_timeout # default is 60s
    }

    # get the raw directory listing in json format from Manta
    # Modified for up to 90s dns or network fails for child subdirectories.
    
    msg <- "mantaFind: Network Error - Sleeping for 5 seconds"
    errorcount <- bunyanTracebackErrors()
    repeat {
      json <- mantaLs(mantapath=curlUnescape(path_enc), l ='json', internal = TRUE)
      newerrors <- bunyanTracebackErrors()
      if (errorcount == newerrors) { 
        # no new errors
        break 
      }   
      errorcount <- newerrors
      if (countdown > 0) { 
        bunyanLog.info(msg)
        if (info == TRUE) {
          cat(paste(msg,"\n"))
        }
        Sys.sleep(5)
        countdown <- countdown - 5
      } else {
        # we timed out, not likely to recover
        msg <- "mantaFind Stopped after timeout - loss of network connectivity to Manta service, check logs for detail"
        bunyanLog.error(msg)
        break 
      }
    }

    if (countdown <= 0) {
       stop(msg)
    }

    if (length(json) == 1) {
        if (json[1] == "") return("")
    }

    # extract the subdirectories as paths for recursive descent
    # using already gathered json above
    subdirs <- mantaLs(mantapath = curlUnescape(path_enc), json=json, l = 'paths', items = 'd', internal = TRUE)

    if (length(subdirs[subdirs != ""]) != 0) {
      # there are subdirectories here
      # are we restricting depth of search with level?
      # level 0 is unrestricted search
      if ((level == 0)  ||  (level > (findroot))) {
        # level is the limit, say 2, findroot is 1 for first pass, 2 for second pass. 
        # recurse through the child subdirectories 
        # no sorting, return R objects that match filters
        for (i in 1:length(subdirs)) {
            msg <- paste("Fetching child path: ", subdirs[i], sep="")
            bunyanLog.info(msg = msg)
            if (info == TRUE) {
              cat(msg,"\n")
            }
          nothing <- mantaFind(subdirs[i], grepfor = grepfor, l = 'R', items = items, level = level, sortby = 'none',
                      findroot = findroot + 1, verbose = verbose)
        }
      }
    }
    
    manta_globals$find_dir_count <- manta_globals$find_dir_count + 1
    manta_globals$find_list[[manta_globals$find_dir_count]] <- list(path = curlUnescape(path_enc), json = json)   
    if (findroot != 1) {
      return()
    } else {
      # This is the top level call to mantaFind
      # convert manta_globals$find_list into structured R entries
      ## Callback 2 onvert JSON chunks into R structure, 
      RNormalize <- function(line, path) {
        entry <- fromJSON(line)
        if (mode(entry) == "character") {
          entry <- as.vector(entry, mode="list")
        }
        if (is.na(charmatch("size",names(entry)))) {
          entry$size <- 0
        }
        # parse the timestamp into R
        time <- strptime(entry$mtime, tz = "GMT", "%Y-%m-%dT%H:%M:%OS")
        # Replace mtime with parsed R time
        entry$mtime <- time
        # prepend the path to the name 
        if (!is.na(charmatch("name",names(entry)))) {
           entry$name <- (paste(path, "/", entry$name, sep=""))
        }
          if (!is.na(charmatch("id",names(entry)))) {
           entry$id <- (paste(path, "/", entry$id, sep=""))
        }
        return(entry)
      }
 
      ## Callback 1 R normalize, size setting, path prepending and time parsing callback
      RstylePath <- function(line) {
        path <- line$path
        path_entries <- lapply(line$json, RNormalize, path=path)
        
      }

      # Parse entries into Rstyle and normalize
      entries <- unlist(lapply(manta_globals$find_list, RstylePath), recursive = FALSE)
      # done with manta_globals$find_list - reclaim that memory
      manta_globals$find_list <- list()
      # R entries now ready ...
    }
  } else {
    # user-supplied R entries made from previous mantaFind call
    if (info == TRUE) {
      cat("Using previously retrieved mantaFind directory\n")
    }
  }

  # Cleanup blank, zero entries
  if (length(entries) == 0) {
    return("")
  }
  entries <- entries[!sapply(entries, is.null)]
  entries <- entries[entries != ""]
  if (length(entries) == 0) {
     return("")
  }

  ## apply filters/grep here.

  ## Regexp filtering of names - strongest filter first..
  # Applies regular expression across entire pathname ....
  if (!missing(grepfor)) {
    names <- unlist(lapply(entries, mantagetnames))
    entries <- entries[grep(grepfor, names, ignore.case=ignore.case, perl=perl )]
    # Anything left?
    if (length(entries) == 0) {
     return("")
    }
  }


  ## Directory entry type filter callback
  filtertype <- function(line) {
    if (items == "d") {
      if (line$type == "directory") {
        return(line)
      }
    }
    if (items == "o") {
      if (line$type == "object") {
        return(line)
      }
    }
  }

  ## item type filtering (directory, file or both)
  if (items != "a") {
    # filter by entry type
    entries <- lapply(entries, filtertype)
    entries <- entries[!sapply(entries, is.null)]
    # Anything left?
    if (length(entries) == 0) {
      return("")
    }
  }


  ## Time window filtering:
  if (timerange == TRUE ) {
    # starttime and endtime set and checked, use to filter find results       
    times <- lapply(entries, mantagettime)
    times <- do.call(c,times) # Keeps the  R time structure, order...
    # extract entries within time window
    # greater than starttime
    times_start <- which(times >= starttime)
    entries <- entries[times_start]
    # Anything left?
    if (length(entries) == 0) { 
      return("")
    }
    times <- times[times_start]
    # extract entries smaller than endtime
    times_end <- which(times <= endtime)
    entries <- entries[times_end]
    # Anything left?
    if (length(entries) == 0) {
      return("")
    }
  }


  ## callback to return size and path style entries
  getsizepath<-function(line) {
    if (!is.na(charmatch("name", names(line)))) {
      name <- (line$name)
    }
    if (!is.na(charmatch("id", names(line)))) {
      name <- (line$id)
    }
   return(list(size = as.numeric(line$size), path = name))
  }
    
  ## callback to return URL style entries
  urlstyle <-function(line) {
    return(paste(manta_globals$manta_url,mantaliststyle(line),sep=""))
  }

  ## set the output format, for numbers, return those
  switch(l,
      R={
        lines <- entries
      },
      paths={ 
      # note we are using the liststyle callback here
      # and the R entries have full paths now embeded in the
      # name field        
        lines <- lapply(entries, mantaliststyle)
      },
      sizes={
        lines <- lapply(entries, mantagetsize)
      },
      size_path={
        lines<- lapply(entries, getsizepath)
      },
      l={
        lines <- lapply(entries, mantaunixstyle)
      },
      URL={
        lines <- lapply(entries, urlstyle)
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

  ## Return results in l format 'paths', 'n', 'du', 'R'
  if ((l == "R") || (l == "size_path")) {  # don't flatten the R stuctures
      return(lines)
  } else { # flatten the lines
      return(do.call(c,lines))
  }  
}
