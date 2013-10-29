# Roxygen Comments mantaSetwdStor
#' Sets Manta working directory to ~~/stor
#'
#' @keywords Manta, manta
#'
#' @export
mantaSetwdStor <-
function() {
  #  Set Manta working directory to ~~/stor
  #
  # Args: none.
  #
  # Returns:
  #
    manta_cwd <- paste("/", manta_globals$manta_user, "/stor", sep="")
    assign("manta_cwd", manta_cwd, envir=manta_globals)
}
