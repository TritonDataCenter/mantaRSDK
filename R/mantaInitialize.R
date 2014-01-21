# Roxygen Comments mantaInititalize
#' Initialize Manta user variables, check SSH key file exists.
#'
#' Not Exported. Initialization of \code{manta_globals} with environment variables
#' checks SSH private key file, sets manta cwd to \code{~~/stor}.
#'
#' @param useEnv logical: \code{TRUE} unless called from \code{\link{mantaAccount}} 
#' - skips getting env/system settings.
#'
#' @return \code{TRUE} or \code{FALSE} on warn, \code{stop} on errors: missing env 
#' variables, SSH key.
#'
#' @keywords Manta
#'
mantaInitialize <-
function(useEnv = TRUE) {
  warn <- FALSE
  bunyanSetLog(level = 'TRACE', logfile = "mantaRSDK.log", memlines = 1000)
  if (useEnv == TRUE) {
    # The default openssl location
    if (.Platform$OS.type == "unix") {
      home <- Sys.getenv("HOME")
      ssl_key_path <- paste(home, "/.ssh/id_rsa", sep="")
    } else { 
      # Windows 
      homedrive <- Sys.getenv("HOMEDRIVE")
      homepath <- Sys.getenv("HOMEPATH")
      ssl_key_path <- paste(homedrive, homepath, "\\.ssh\\id_rsa", sep="")
    }
    if (file.exists(ssl_key_path) != TRUE) {
      msg <- paste("mantaInitialize - private key not found at:", "\n", ssl_key_path,
          "\n See help(mantaAccount) help(mantaWhoami)", sep="")
      cat(msg)
      bunyanLog.error(msg = msg)
    }
    manta_user <- Sys.getenv("MANTA_USER")
    if (nchar(manta_user) == 0) {
      msg <- "mantaInitialize environment variable not found: MANTA_USER"
      bunyanLog.error(msg = msg)
      stop(msg)
    }
    manta_key_id <- Sys.getenv("MANTA_KEY_ID")
    if (nchar(manta_key_id) == 0) {
      msg <- "mantaInitialize environment variable not found: MANTA_KEY_ID"
      bunyanLog.error(msg = msg)
      stop(msg)
    }
    manta_url <- Sys.getenv("MANTA_URL")
    if (nchar(manta_user) == 0) {
      msg <- "mantaInitialize environment variable not found: MANTA_URL"
      bunyanLog.error(msg = msg)
      stop(msg)
    }
    manta_key_path <- paste("/",manta_user,"/keys/",manta_key_id, sep="")
    manta_cwd <- paste("/",manta_user, "/stor", sep="")
    assign("manta_user", manta_user, envir=manta_globals)
    assign("manta_key_id", manta_key_id, envir=manta_globals)
    assign("manta_key_path", manta_key_path, envir=manta_globals)
    assign("manta_url", manta_url, envir=manta_globals)
    assign("manta_cwd", manta_cwd, envir=manta_globals)
    assign("ssl_key_path", ssl_key_path, envir=manta_globals)
  } else {
    # This is an update call from mantaAccount - requires all six values set.
    if (nchar(manta_globals$manta_user) == 0) {
      msg <- "mantaAccount:mantaInitialize Error - No initial Manta username set.\n See help(mantaAccount)\n"
      bunyanLog.error(msg = msg)
      stop(msg)
    }
    if (nchar(manta_globals$manta_key_id) == 0) {
      msg <- "mantaAccount:mantaInitialize Error - No initial Manta Key ID set.\n See help(mantaAccount)\n"
      bunyanLog.error(msg = msg)
      stop(msg)
    }
    if (nchar(manta_globals$manta_url) == 0) {
      msg <- "mantaAccount:mantaInitialize Error - No initial Manta URL set.\n See help(mantaAccount)\n"
      bunyanLog.error(msg = msg)
      stop(msg)
    }
    if (nchar(manta_globals$ssl_key_path) == 0) {
      msg <- "mantaAccount:mantaInitialize Error - No initial SSH key location set.\n See help(mantaAccount)\n"
      bunyanLog.error(msg = msg)
      stop(msg)
    }
    if (nchar(manta_globals$manta_key_path) == 0) {
      msg <- "mantaAccount:mantaInitialize Internal Error - Manta key path not set.\n See help(mantaAccount)\n"
      bunyanLog.error(msg = msg)
      stop(msg)
    }
    if (nchar(manta_globals$manta_cwd) == 0) {
      msg <- "mantaAccount:mantaInitialize Internal Error - Manta cwd not set.\n See help(mantaAccount)\n"
      bunyanLog.error(msg = msg)
      stop(msg)
    }
    # Find the specified SSH key file or warn user to fix it...
    if (file.exists(manta_globals$ssl_key_path) != TRUE) {
      msg <- paste("mantaAccount:mantaInitialize - Warning - private SSH key file not found at:", 
                 "\n", manta_globals$ssl_key_path, "\n See help(mantaAccount)\n", sep="")
      bunyanLog.warn(msg = msg)
      cat(msg)
      warn <- TRUE
    }
  }

  options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

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

  # Error classes retrieved from ruby-manta API - some strings truncated 
  # e.g. AuthorizationFailed is  Authorization in docs,
  #    AuthSchemeNotAllowed is AuthScheme in docs

  manta_error_classes <- c("Authorization", "AuthScheme",
                       "BadRequest", "Checksum", "ConcurrentRequest",
                       "ContentLength", "ContentMD5Mismatch",
                       "DirectoryDoesNotExist", "DirectoryExists",
                       "DirectoryNotEmpty", "DirectoryOperation",
                       "EntityExists", "Internal", "InvalidArgument",
                       "InvalidAuthToken", "InvalidCredentials",
                       "InvalidDurabilityLevel", "InvalidJob", "InvalidKeyId",
                       "InvalidLink", "InvalidSignature", "InvalidJobState",
                       "JobNotFound", "JobState", "KeyDoesNotExist",
                       "LinkNotFound", "LinkNotObject", "LinkRequired",
                       "NotAcceptable", "NotEnoughSpace", "ParentNotDirectory",
                       "PreconditionFailed", "PreSignedRequest",
                       "RequestEntityTooLarge", "ResourceNotFound",
                       "RootDirectory", "SecureTransportRequired",
                       "ServiceUnavailable", "SourceObjectNotFound",
                       "SSLRequired", "TaskInit", "UploadTimeout",
                       "UserDoesNotExist", "UserTaskError",
                       # and errors that are specific to this class:
                       "CorruptResult", "UnknownError",
                       "UnsupportedKey") 

  assign("manta_error_classes", manta_error_classes, envir=manta_globals)

  # some functions to get / set these are in order when they are 
  # actually in use.
  manta_defaults <- list(durability_level = 2, connect_timeout = 5, send_timeout = 60, 
                         receive_timeout = 60, max_limit = 1000)

  assign("manta_defaults", manta_defaults, envir=manta_globals)

  manta_methods <- c("GET", "POST", "PUT", "DELETE", "OPTIONS", "HEAD")
  assign("manta_methods", manta_methods, envir=manta_globals)
  
  r_version <- as.character(getRversion())
  r_version <- paste("R-",r_version, sep="")
  user_agent <- paste(r_version, "/mantaRSDK", sep="")
  assign("user_agent", user_agent, envir=manta_globals)
  assign("r_version", r_version, envir=manta_globals)
  session <- sessionInfo()
  RSDK_version <- session$otherPkgs$mantaRSDK$Version
  assign("RSDK_version", RSDK_version, envir=manta_globals)
  hostname <- as.character(Sys.info()["nodename"])
  assign("hostname", hostname, envir=manta_globals)

  # If we made it this far, we have values for everything. 
  if (warn == TRUE) {     
     assign("manta_initialized", FALSE, envir=manta_globals)
     bunyanLog.info("mantaRSDK failed to initialize")
     return(FALSE)
  } else {
     assign("manta_initialized", TRUE, envir=manta_globals)
     bunyanLog.info("mantaRSDK initialized")
     return(TRUE)
  }
}
