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

    manta_cwd <- "~~/stor"

    path_enc <- mantaExpandPath(manta_cwd)
    assign("manta_cwd", path_enc, envir=manta_globals)
}
