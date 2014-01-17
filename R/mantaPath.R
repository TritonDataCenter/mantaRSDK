# Roxygen Comments mantaPath
#' Given a user typed path or object name, returns full path, 
#' does not validate object is there, assumes it is in current subdir.
#' Not exported.
#'
#' @param m_path character, required. Manta object 
#'
#' @return character with full Manta path to object or empty string ""
#' as processed by curlEscape with / left in 
#' 
#' @keywords Manta, manta
#'
#'
mantaPath <-
function(m_path) {
    path_enc <- mantaExpandPath(m_path)
    if (path_enc == "") {
      # no valid path prefix, assume off working subdir
      path <- paste(mantaGetwd(), m_path, sep ="/")
      path <- sub("//","/",path) # if user already put in / added by sep above
      path_enc <- mantaExpandPath(path)
    }
    return(path_enc)
}

