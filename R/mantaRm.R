# Roxygen Comments mantaRm
#' Removes specified Manta object, optionally recursive, not vectorized.
#'
#' Removes object. Specify absolute (e.g. \code{~~/stor/myobject.txt} )
#' or relative path from the current Manta directory. Supports
#' recursive removal of child contents objects and directories 
#' with \code{r = TRUE}.
#' You can use \code{mantaFind} to prepare a list of absolute Manta 
#' pathnames to objects with detailed
#' searching and filtering specifications and then use 
#' \code{lapply(pathnamelist, mantaRm)} to delete the items.
#'
#' @param mantapath character, required. Not vectorized.
#' 
#' @param r, logical optional. Set \code{TRUE} for recursive delete 
#' of all objects within all child subdirectories, 
#' and the directories.
#'
#' @param info logical. Set FALSE to suppress console messages.
#'
#' @keywords Manta, manta
#'
#' @return \code{TRUE} or \code{FALSE} depending on success of remove.
#'
#' @family Directory
#'
#' @examples
#' \dontrun{
#' data <- runif(100)
#' mantaDump("data")
#' mantaCat("dumpdata.R")
#' mantaRm("dumpdata.R")
#'
#' ## Make a hierarchical directory set, for removal:
#' mantaGetwd() -> tempdir
#' mantaMkdir("~~/stor/a_test/b_test/c_test", p = TRUE)
#' mantaSetwd("~~/stor/a_test/b_test/c_test")
#' mantaMkdir("d_test")   # Relative path
#' mantaDump("data_c")
#' mantaSetwd("..")
#' mantaDump("data_b")
#' mantaSetwd("..")
#' mantaDump("data_a")
#' mantaLs.l()
#' mantaFind()
#'
#' ## Recursive Rm Subdirectory Contents
#' mantaSetwd.stor()
#' mantaRm("~~/stor/a_test", r = TRUE)
#' mantaSetwd("~~/stor/a_test")
#' mantaLs.l()
#' mantaFind()
#' mantaSetwd.stor()
#' mantaRmdir("~~/stor/a_test")
#' mantaLs.l("~~/stor")
#' mantaSetwd(tempdir)
#' }
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

  silent <- !info

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
