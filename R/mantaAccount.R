#
# Roxygen Comments mantaAccount
#' Changes current Manta account information 
#'
#' The Manta account is initially obtained from
#' three environment variables:\cr
#'  \code{$MANTA_USER, $MANTA_KEY, $MANTA_URL}.\cr 
#' The ssl key location is obtained by default on Unix/Linux
#' from\cr
#' \code{/$HOME/.ssh/id_rsa}\cr  
#' or on Windows from:\cr 
#' \code{C:\\Users\\username\\.ssh\\ir_rsa}\cr
#' The Manta datacentre enviroment variable is:\cr
#'  \code{$MANTA_URL}
#'
#'
#' @param account list,  optional. Input R account values.
#'
#' @param json character, optional. Input JSON account values.
#'
#' @param verbose logical, optional. Show HTTP communication. 
#' \code{FALSE} by default.
#'
#' @return logical TRUE if account changed and working. Reverts to
#' previous working account if it cannot connect wit the new information
#' returns FALSE for both cases - account reverted or account is left in
#' a state where it cannot communicate to the server.
#'
#' @family mantaAccount
#'
#' @seealso \code{\link{mantaWhoami}}
#'
#' @examples
#' \dontrun{
#' ## To see/save current account settings:
#' account <- mantaWhoami(all = TRUE)
#'
#' ## then use:  
#'
#' mantaAccount(account) ## to set the modified account
#'
#' ## Account information may contain 1-4 key-value pairs.
#'
#' ## To see/save current account settings as JSON:
#'
#' account <- mantaWhoami(all = TRUE, json = TRUE)
#'
#' ## then use:  
#'
#' mantaAccount(json = account) to set that account
#'
#' ## To set a non default SSH private key location on Windows
#'
#' mysslkey <- list(SSL_KEY_PATH = "C:\\Users\\myacct\\.ssh\\my_priv_rsa")
#' mantaAccount(mysslkey) 
#' }
#'
#' @keywords Manta, manta
#'
#' @export
mantaAccount <-
function(account, json, verbose=FALSE) {

  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

  if (missing(account) && missing(json)) {
      stop("mantaAccount: No account information provided.\nSee: help(mantaAccount)\n")
  }

  if (missing(account)) {
    # do we have JSON input? 
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
  
  backup_working <- FALSE
  if (manta_globals$manta_initialized == TRUE) {
    if (mantaAttempt(test = TRUE, verbose = FALSE) == TRUE) { backup_working <- TRUE }
    backup <- mantaWhoami(all = TRUE, json = TRUE)
    backup_wd <- mantaGetwd()
  }
  
  
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
  new_key_path <- paste("/",manta_globals$manta_user,"/keys/",manta_globals$manta_key_id, sep="")
  assign("manta_key_path", new_key_path, envir = manta_globals)

  if (manta_globals$manta_initialized == TRUE) {
    if (verbose == TRUE) {
      cat("Changing account from:\n")
      cat(backup)
      cat("\n\n to ")
    }
  } 
  
  if (verbose == TRUE) {
      cat("account settings: \n")
      cat(mantaWhoami(all = TRUE, json = TRUE))
      cat("\n")
      cat("Manta working directory is:\n")
      cat(manta_globals$manta_cwd)
      cat("\n")
  }

  # Call mantaInitialize to force check of values 
  if (mantaInitialize(useEnv = FALSE) == TRUE) {
    # see if we get a connection, report any errors to console
    if (mantaAttempt(test= TRUE, verbose = verbose) == TRUE) {
      return(TRUE)
    }
  }

  # These new settings don't work so revert to working backup settings
  if (backup_working == TRUE) {
     mantaAccount(json=backup, verbose = verbose)
     mantaSetwd(backup_wd)
  } else {  # backup settings not working, nor new settings...
     cat("Your Manta Account Settings are not properly configured\n")
     cat("Use mantaWhoami(all=TRUE) to inspect your settings.\n ")
  }

  return(FALSE)  # We did not successfully change the account.

}
