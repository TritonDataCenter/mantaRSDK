
# Roxygen Comments mantaInit
#' Initialize Manta and test; use optional private key file
#'
#' Allows for user-specified SSL key file lookups.
#' Calls internal \code{\link{mantaInitialize}}
#'
#' @param private_key_path string, optional. Absolute path to 
#' private SSL key file as registered with user's 
#' Joyent Manta account.
#' If \code{private_key_path} is not provided, looks for
#' default OpenSSL key at /$HOME/.ssh/id_rsa
#'
#' @param verbose logical, passed through to RCurl
#'
#' @return TRUE, otherwise stop() on errors.
#'
#' @keywords Manta, manta
#'
#' @export
#'
#' @examples
#' mantaInit()
#'
mantaInit <-
function(private_key_path, verbose = FALSE) {
  # TODO: 
  #   Windows ssl key goes here
  #   Add in explicit values for MANTA_ values?
  #   mls handshake test
  #   figure out true/false logic  

  if (manta_globals$manta_ok == FALSE) {
     mantaInitialize(private_key_path)
  }
  
  # Test call to Manta 
  json_test <- mantaAttempt(manta_globals$manta_cwd,
                            json = TRUE,
                            verbose = verbose)
  # Figure out true/false logic
}
