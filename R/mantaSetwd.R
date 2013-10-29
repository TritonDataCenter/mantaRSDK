# Roxygen Comments mantaSetwd
#' Sets Manta working directory 
#'
#' @param mantapath string, required. 
#'
#' @keywords Manta, manta
#'
#' @export
mantaSetwd <-
function(mantapath) {
    assign("manta_cwd", mantapath, envir=manta_globals)
}
