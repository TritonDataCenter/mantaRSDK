# Roxygen Comments mantaInititalize
#' Initialize Manta user variables, check SSL key file exists
#'
#' Initialization of manta_globals with environment variables
#' checks SSL private key file, sets manta cwd to ~~/stor
#'
#' @param useEnv logical: TRUE unless called from mantaAccount - skips getting env/system settings
#'
#' @return TRUE or stop() on errors: missing env variables, SSL key
#'
#' @keywords Manta, manta
#'
mantaInitialize <-
function(useEnv = TRUE) {
  if (useEnv == TRUE) {
    # The default openssl location
    home <- Sys.getenv("HOME")
    ssl_key_path <- paste(home, "/.ssh/id_rsa", sep="")
    if (file.exists(ssl_key_path) != TRUE) {
      stop(paste("mantaRSDK:mantaInitialize configuration error - private key not found at:", 
                 "\n", ssl_key_path, sep=""))
    }
    manta_user <- Sys.getenv("MANTA_USER")
    if (nchar(manta_user) == 0) {
      stop("mantaRSDK:mantaInitialize environment variable not found: MANTA_USER")
    }
    manta_key_id <- Sys.getenv("MANTA_KEY_ID")
    if (nchar(manta_key_id) == 0) {
      stop("mantaRSDK:mantaInitialize environment variable not found: MANTA_KEY_ID")
    }
    manta_url <- Sys.getenv("MANTA_URL")
    if (nchar(manta_user) == 0) {
      stop("mantaRSDK:mantaInitialize environment variable not found: MANTA_URL")
    }
    manta_key_path <- paste("/",manta_user,"/keys/",manta_key_id, sep="")
    manta_cwd <- paste("/", manta_user, "/stor", sep="")
    assign("manta_user", manta_user, envir=manta_globals)
    assign("manta_key_id", manta_key_id, envir=manta_globals)
    assign("manta_key_path", manta_key_path, envir=manta_globals)
    assign("manta_url", manta_url, envir=manta_globals)
    assign("manta_cwd", manta_cwd, envir=manta_globals)
    assign("ssl_key_path", ssl_key_path, envir=manta_globals)
  } else {
    # This is a call from mantaAccount - requires all six values set.
    if (nchar(manta_globals$manta_user) == 0)
      stop("mantaRSDK:mantaAccount:mantaInitialize Error - No initial Manta username set.\n See help(mantaAccount)\n")
    if (nchar(manta_globals$manta_key_id) == 0)
      stop("mantaRSDK:mantaAccount:mantaInitialize Error - No initial Manta Key ID set.\n See help(mantaAccount)\n")
    if (nchar(manta_globals$manta_url) == 0)
      stop("mantaRSDK:mantaAccount:mantaInitialize Error - No initial Manta URL set.\n See help(mantaAccount)\n")
    if (nchar(manta_globals$ssl_key_path) == 0)
      stop("mantaRSDK:mantaAccount:mantaInitialize Error - No initial SSH key location set.\n See help(mantaAccount)\n")
    if (nchar(manta_globals$manta_key_path) == 0)
      stop("mantaRSDK:mantaAccount:mantaInitialize Internal Error - Manta key path not set.\n See help(mantaAccount)\n")
    if (nchar(manta_globals$manta_cwd) == 0)
      stop("mantaRSDK:mantaAccount:mantaInitialize Internal Error - Manta cwd not set.\n See help(mantaAccount)\n")
    # must find the specified SSH key file or stop and fix it...
    if (file.exists(manta_globals$ssl_key_path) != TRUE) {
      stop(paste("mantaRSDK:mantaAccount:mantaInitialize - Configuration error - private key not found at:", 
                 "\n", manta_globals$ssl_key_path, sep=""))
    }
  }

  # If this gives trouble in the future may need to add to JSON for mantaAccount
  manta_cainfo <- system.file("CurlSSL/cacert.pem", package = "RCurl")
  assign("manta_cainfo", manta_cainfo, envir=manta_globals)

  # If we made it this far, we have values for everything. 
  assign("manta_initialized", TRUE, envir=manta_globals)

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
