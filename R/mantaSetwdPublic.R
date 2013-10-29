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
    manta_cwd <- paste("/", manta_globals$manta_user, "/public", sep="")
    assign("manta_cwd", manta_cwd, envir=manta_globals)
}
