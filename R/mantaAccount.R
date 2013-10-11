# TODO.  Read account information from saved file
#        Refactor mantaInit/mantaInitialize to be called from mantaAccount 
#        for Windows support
#        Test mantademo / cwvhogue switching Unix/Windows
#
# Roxygen Comments mantaAccount
#' Changes current Manta account information 
#'
#' The Manta account is initially obtained from
#' environment variables $MANTA_USER, $MANTA_KEY, $MANTA_URL. 
#' The ssl key location is obtained by default on Unix/Linux
#' from /$HOME/.ssh/id_rsa  OR as set by user with mantaInit().
#' The Manta datacentre enviroment variable is $MANTA_URL
#'
#' To see/save current account settings:
#'
#' account <- mantaWhoami(all = TRUE)
#'
#' then use:  
#'
#' mantaAccount(account) to set that account
#'
#' Account information may contain 1-4 key-value pairs.
#'
#' To see/save current account settings as JSON:
#'
#' account <- mantaWhoami(all = TRUE, json = TRUE)
#'
#' then use:  
#'
#' mantaAccount(json = account) to set that account
#'
#' @param account list,  optional. R values 
#'
#' @param json character, optional. JSON account values
#'
#' @param verbose, logical, optional. TRUE by default
#'
#' @return logical, TRUE if account changed, stop on errors.
#'
#' @keywords Manta, manta
#'
#' @export
mantaAccount <-
function(account, json, verbose=TRUE) {
  if (missing(account) && missing(json)) {
      stop("No Manta account information provided.\nSee: help(mantaAccount)\n")
  }

  if (missing(account)) {
    # we have JSON input 
    if (isValidJSON(json, asText = TRUE)) { 
       account <- fromJSON(json)
    } else {
       stop(paste("Invalid JSON content:\n",json,"\n",sep=""))
    }
  }

  # Check account contents
  headings <- names(account)
  if (length(headings) == 0) 
    stop("Incorrect Manta account structure\nSee: help(mantaAccount)\n")

  backup <- mantaWhoami(all = TRUE, json = TRUE)
  
  # Fetch and assign new values as provided

  new_user <- ""
  if (is.na(charmatch("MANTA_USER" ,headings)) == FALSE) {
    new_user <- as.character(account[charmatch("MANTA_USER" ,headings)])
    assign("manta_user", new_user , envir = manta_globals)
  }

  new_key_id <- ""
  if (is.na(charmatch("MANTA_KEY_ID", headings)) == FALSE) {
    new_key_id <- as.character(account[charmatch("MANTA_KEY_ID", headings)])
    assign("manta_key_id", new_key_id, envir = manta_globals)
  }

  new_url <- ""
  if (is.na(charmatch("MANTA_URL", headings)) == FALSE) {
    new_url <- as.character(account[charmatch("MANTA_URL",headings)])
    assign("manta_url", new_url, envir = manta_globals)
  }

  if (is.na(charmatch("SSL_KEY_PATH", headings)) == FALSE) {
    new_ssl <- as.character(account[charmatch("SSL_KEY_PATH", headings)])
    assign("ssl_key_path", new_ssl, envir = manta_globals)
  }

  # A change in user or datacentre - invalidates manta_globals$manta_cwd
  # so it is reset to ~~/stor
  if ((new_user != "") || (new_url != "")) {
    manta_cwd <- paste("/", manta_globals$manta_user, "/stor", sep="")
    assign("manta_cwd", manta_cwd, envir = manta_globals)
  }

  # A change in user or key_id  invalidates manta_globals$manta_key_path 
  if ((new_user != "") || (new_key_id != "")) {
    new_key_path <- paste("/",new_user,"/keys/",new_key_id, sep="")
    assign("manta_key_path", new_key_path, envir = manta_globals)
  }

  if (verbose == TRUE) {
    cat("Account Changed From:\n")
    cat(backup)
    cat("\n\n To:\n")
    cat(mantaWhoami(all = TRUE, json=TRUE))
    cat("\n")
    cat("Manta working directory is:\n")
    cat(manta_globals$manta_cwd)
    cat("\n")
  }

  return(TRUE)

}
