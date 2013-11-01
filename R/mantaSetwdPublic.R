# Roxygen Comments mantaSetwdPublic
#' Sets Manta working directory to ~~/public
#'
#' @keywords Manta, manta
#'
#' @export
mantaSetwdPublic <-
function() {
  # Set Manta working directory to ~~/public
  #
  # Args:
  #
  # Returns:
  #
    manta_cwd <- "~~/public"
    path_enc <- mantaExpandPath(manta_cwd)
    assign("manta_cwd", path_enc, envir=manta_globals)
}
