# Roxygen Comments mantaRm
#' Removes Manta object specified by full manta path or from current
#' working manta directory. Option r = TRUE does recursive delete
#' of object and subdirectories.
#' Returns TURE if object successfully removed
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
    cat("mantaRm Error - no Manta object or path to object specified")
    return(FALSE)
  }

  path_enc <- mantaPath(mantapath)

  if (path_enc == "") return(FALSE)

  if (r == FALSE) {
    # non recursive
    return(mantaAttempt(action=path_enc, method="DELETE", test = TRUE, returncode="204"))
  } else {
    # recursive
    # Call mantaFind to get tree
    if (info == TRUE) {
     cat("Gathering subdirectory structure for deletion...\n")
    }
    tree <- mantaFind(l='R', items='a', info=info)
    errors <- bunyanTracebackN(level='ERROR')
    if (errors > 0) {
      msg = paste("mantaRm: mantaFind ",errors, " transmission errors encountered, delete aborted\n",
                  "View mantaRSDK log or try bunyanTraceback()\n",sep="")
      bunyanLog.error(msg)
      cat(msg)
      return(FALSE)
    }

    # Watch for errors encountered
    bunyanClearSetpoint()
    bunyanSetpoint()

    # default tree is returned in depth-first order which is 
    # already in proper delete order, empty dirs are first on the list
    # then objects.
  
    # Delete callback
    deletefunction <- function(line) {
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
             bunyanLog.error(msg)
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
             bunyanLog.error(msg)
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
             bunyanLog.error(msg)
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
             bunyanLog.error(msg)
             cat(msg,"\n")
           }
         }
       }
    } 

    # Go through the tree one at a time...
    if (tree[1] != "") {
      lapply(tree, deletefunction)
      errors <- bunyanTracebackN(level='ERROR')
      if (errors == 0) {
        return(TRUE)   
      } else {
        # errors encountered
        msg = paste("mantaRm: Recursive delete failed to remove ", errors, "  entries\n",
                   "View mantaRSDK log or try bunyanTraceback()\n",sep="")
        bunyanLog.error(msg)
        cat(msg)
        return(FALSE)
      }
    } else {
      return(FALSE) # no entries
    }
  }
}
