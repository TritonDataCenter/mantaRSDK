# Roxygen Comments mantaSnapln
#' Snaplinks are combination snapshots and symbolic links. 
#' If the original object is overwritten/deleted, the SnapLink
#' still contains the object contents at time of creation.
#'
#' @param from string, requireed. Object in current subdirectory
#' or full Manta path to stored object.
#'
#' @param to string, requireed. Snaplink name in current subdirectory
#' or full Manta path to the new SnapLink..
#'
#' @keywords Manta, manta
#'
#' @export
mantaSnapln <-
function(from, to) {
 if (missing(from) || missing(to)) {
     cat("mantaRSDK:mantaSnapln Error - Missing argument - from or to")
     return(FALSE)
    }

    from_path_enc <- mantaExpandPath(from)
    if (from_path_enc == "") {
      # no valid path prefix, assume the cwd prefix intended
      path <- paste(mantaGetwd(), from, sep ="/")
      path <- sub("//","/",path) # if user already put in / added by sep above
      from_path_enc <- mantaExpandPath(path)
    }

    to_path_enc <- mantaExpandPath(to)
    if (to_path_enc == "") {
      # no valid path prefix, assume the cwd prefix intended
      path <- paste(mantaGetwd(), to, sep ="/")
      path <- sub("//","/",path) # if user already put in / added by sep above
      to_path_enc <- mantaExpandPath(path)
    }


    if ((from_path_enc != "") && (to_path_enc != "")) {
     headers <- c('content-type' = "application/json; type=link",
                 'Location' = from_path_enc)
     return(mantaAttempt(action=to_path_enc, method = "PUT", headers = headers, test = TRUE, returncode = "204"))
    }

    return(FALSE)
}

