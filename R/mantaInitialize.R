# Roxygen Comments mantaInititalize
#' Initialize Manta user variables, check SSL key file exists
#'
#' Initialization of manta_globals with environment variables
#' checks SSL private key file, sets manta cwd to ~~/stor
#'
#' @param pk Absolute path to user's private SSL key file as
#' registered with Joyent Manta account \code{pk}
#'
#' @return TRUE or stop() on errors: missing env variables, SSL key
#'
#' @keywords Manta, manta
#'
mantaInitialize <-
function(pk) {
  if (missing(pk)) {
    # The default openssl location
    home <- Sys.getenv("HOME")
    pk_path <- paste(home, "/.ssh/id_rsa", sep="")
  } else {
    pk_path <- pk
  }
  if (file.exists(pk_path) != TRUE) {
      stop(paste("Manta configuration error - private key not found at:", 
                 "\n", pk_path, sep=""))
  }
  manta_user <- Sys.getenv("MANTA_USER")
  if (nchar(manta_user) == 0) {
    stop("Manta environment variable not found: MANTA_USER")
  }
  manta_key_id <- Sys.getenv("MANTA_KEY_ID")
  if (nchar(manta_key_id) == 0) {
    stop("Manta environment variable not found: MANTA_KEY_ID")
  }
  manta_url <- Sys.getenv("MANTA_URL")
  if (nchar(manta_user) == 0) {
    stop("Manta environment variable not found: MANTA_URL")
  }
  manta_key_path <- paste("/",manta_user,"/keys/",manta_key_id, sep="")
  manta_cwd <- paste("/", manta_user, "/stor", sep="")
  assign("manta_user", manta_user, envir=manta_globals)
  assign("manta_key_path", manta_key_path, envir=manta_globals)
  assign("manta_url", manta_url, envir=manta_globals)
  assign("manta_cwd", manta_cwd, envir=manta_globals)
  assign("pk_path", pk_path, envir=manta_globals)
  assign("manta_ok", TRUE, envir=manta_globals)

  # Interactive support with Node.js?
  nodejs_path<-Sys.which("node")
  if (nchar(nodejs_path) != 0) {
    nodejs_version <- system2("node", args = "--version", stdout=TRUE)
    nodejs_mlogin <- Sys.which("mlogin")
    if (nchar(nodejs_mlogin) != 0) {
      assign("nodejs_version", nodejs_version, envir=manta_globals)
      assign("manta_node", TRUE, envir=manta_globals)
    }
  } else {
    assign("manta_node", FALSE, envir=manta_globals)
#    cat("Tip: Install Node.js & manta-node \n")
#    cat("     for Manta Interactive R Sessions\n")
#    cat("     with mantaLoginR() and mantaLoginBash()\n")
  }

  # retrieve variables with 
  # get('manta_user', manta_globals) or manta_globals$manta_user  
  return(TRUE)
}
