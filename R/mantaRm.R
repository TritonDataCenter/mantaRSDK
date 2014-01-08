# Roxygen Comments mantaRm
#' Removes Manta object specified by full manta path or from current
#' working manta directory. 
#'
#' Option r = TRUE does recursive delete
#' of object and subdirectories.
#' Returns TURE if object and tree successfully removed without warnings/errors
#' You can use mantaFind to prepare a list of pathnames to objects with detailed
#' searching and filtering specifications and then use lapply(pathnamelist, mantaRm) 
#' to delete the items.
#'
#' @param mantapath string, required.
#' 
#' @param r, logical optional. Set TRUE for recursive delete of all objects
#  within all child subdirectories, and the directories.
#'
#' @param info, logical. Show progress information on console.
#'
#' @keywords Manta, manta
#'
#' @export
mantaRm <-
function(mantapath, r = FALSE, info = TRUE) {

  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

  if (missing(mantapath)) {
    cat("mantaRm Error - no Manta object or path to object specified\n")
    return(FALSE)
  }

  path_enc <- mantaPath(mantapath)

  if (path_enc == "") return(FALSE)

  silent <- FALSE
  if (info == FALSE) silent <- TRUE

  if (r == FALSE) {
    # non recursive
    return(mantaAttempt(action=path_enc, method="DELETE", test = TRUE, silent = silent, returncode="204"))
  } else {
    # recursive
    # Call mantaFind to get tree
    if (info == TRUE) {
     cat("Gathering subdirectory structure for deletion...\n")
    }
    tree <- mantaFind(mantapath = mantapath,  l='R', items='a', info=info)
    bunyanClearSetpoint()
    bunyanSetpoint()

    # default tree is returned in depth-first order which is 
    # already in proper delete order, empty dirs are first on the list
    # then objects.
  
    # Delete callback
    deletefunction_do <- function(line) {
      # is this an object or directory
       if (line$type == "directory") {
         if (!is.na(charmatch("name", names(line)))) {
           if (mantaRmdir(mantapath = line$name) == TRUE) {
             if (info == TRUE) { 
               msg = paste("Removed directory ", line$name, sep="")
               bunyanLog.info(msg)
               cat(msg,"\n")
             } 
           } else {
             msg = paste("mantaRm: Unable to remove directory ", line$name, sep="")
             bunyanLog.warn(msg)
             cat(msg,"\n")
           }
         }
         if (!is.na(charmatch("id", names(line)))) {
           if (mantaRmdir(mantapath = line$id) == TRUE) {
             if (info == TRUE) {
               msg = paste("Removed directory ", line$id)
               bunyanLog.info(msg)
               cat(msg,"\n")
             }
           } else {
             msg = paste("mantaRm: Unable to remove directory ", line$id, sep="")
             bunyanLog.warn(msg)
             cat(msg,"\n")
           }
         }
       }   
       if (line$type == "object") {
         if (!is.na(charmatch("name", names(line)))) {
           if (mantaRm(mantapath = line$name) == TRUE) {
             if (info == TRUE) {
               msg = paste("Removed object ", line$name, sep="")
               bunyanLog.info(msg)
               cat(msg,"\n")
             }
           } else {
             msg = paste("mantaRm: Unable to remove object ", line$name, sep="")
             bunyanLog.warn(msg)
             cat(msg,"\n") 
           }
         }
         if (!is.na(charmatch("id", names(line)))) {
           if (mantaRm(mantapath = line$id) == TRUE) {
             if (info == TRUE) {
               msg = paste("Removed object ", line$id,"\n", sep="")
               bunyanLog.info(msg)
               cat(msg,"\n")
             }
           } else {
             msg = paste("mantaRm: Unable to remove object ", line$id, sep="")
             bunyanLog.warn(msg)
             cat(msg,"\n")
           }
         }
       }
    } 

    deletefunction_try <- function(line) {
      timeout <- manta_globals$manta_defaults$receive_timeout  # default is 60
      msg <- "mantaRm Network Transmission Error encountered - Sleeping for 5 seconds"
      errorcount <- bunyanTracebackErrors()
      repeat {
        deletefunction_do(line) 
        #Errors come from mantaAttempt - are transmission errors, Warnings are from from deletefunction_do
        newerrors <- bunyanTracebackErrors()
        if (errorcount == newerrors) {
          # no new errors, warnings are allowed 
          # (e.g. if taks 2 puts object into tree subdir that is not yet deleted by this task 1)
          break
        }
        errorcount <- newerrors
        if (timeout > 0) {
          bunyanLog.info(msg)
          if (info == TRUE) {
            cat(paste(msg,"\n"))
          }
          Sys.sleep(5)
          timeout <- timeout - 5
        } else {
           # Network access is compromised, so we stop
           msg <- "mantaRm Stopped - Network Timeout.  Check logs for details."
           bunyanLog.error(msg)
           stop(msg)
        }
      }
    }

    # Go through the tree one at a time...
    if (tree[1] != "") {
      lapply(tree, deletefunction_try)
      errors <- bunyanTracebackErrors()
      warns <- bunyanTracebackWarnings()  #warnings is an R function -reseved word. like log...
      if ((errors > 0) || (warns > 0)) {
        # errors or warnings encountered
        msg = paste("mantaRm: [", errors, "] Errors and [", warns, "] Warnings encountered. \n",
                   "Intended mantaRm operation may be incomplete.",sep="")
        bunyanLog.info(msg)
        cat(paste(msg,"\n"))
        return(FALSE)
      } else {
        return(TRUE)
      }
    } else {
      return(FALSE) # no entries
    }
  }
}
