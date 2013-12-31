# Roxygen Comments mantaJob.launch
#' Submits list of input files and R format Manta Job specification.
#'
#' Job is created by mantaJob.setup() and tasks as defined therein by
#' mantaMap(), and/or mantaReduce() functions. Note that Manta tasks
#' are UNIX shell commands, not native R commands. 
#'
#' Example - Manta Map/Reduce UNIX Word Count:
#'
#' jobid <- mantaJob.launch( 
#'    inputs = mantaLs.paths("~~/public/shakespeare", grepfor = "[.]txt"), 
#'    job = mantaJob.setup(
#'             name = "word count",
#'             mantaMap("wc"),
#'             mantaReduce("awk '{ l += $1; w += $2; c += $3 } END { print l, w, c }'")
#'          )
#' )
#'
#' mantaJob.status(jobid) # check to see if job is complete
#' mantaJob.output(jobid) # retrieve list of paths to Manta output objects
#' mantaJob.errors(jobid) # retrirve JSON formatted job error information
#
#' @param inputs, optional. List of inputs as a vector of character, each containing 
#' valid paths to Manta objects that are the intended job input files. You may use
#' output from mantaFind() or mantaLs.paths() here.
#'
#' @param job, required. The R job structure as created with mantaJob.Setup() and
#' Map and Reduce job tasks as defined therein by one or more mantaMap() and/or
#' mantaReduce() steps. 
#' More information and parameters are explained in the help for these three functions. 
#'
#' @param verbose logical optional. Passed to RCURL to reveal HTTP communication.
#'
#' @param inputbatch numeric. Maximum number of input object paths to upload
#' in one batch to the running job. Default is 500.
#' 
#' @return Returns a Manta Job identifier, which is a hash that looks like this:
#' "70c30bab-873b-66da-ebc8-ced12bd35ac4". This is used by other mantaJob functions
#' for information, error and output retrieval as a lookup key. This key can
#' also be used by Node.js Manta command-line mjob commands. 
#'
#' @keywords Manta, manta
#'
#' @export
mantaJob.launch <-
function(inputs, job, inputbatch = 500, verbose = TRUE) {

  if (missing(job)) 
    stop("mantaJob.launch - no job specified")

  if (isValidJSON(toJSON(job), asText = TRUE) == FALSE) 
    stop("mantaJob.launch - Job specified is not valid as JSON.  Use output from mantaJob.setup")

  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }

  manta_call <- paste(manta_globals$manta_url,"/",manta_globals$manta_user,"/jobs", sep="")
  httpheader <- mantaGenHeaders()
  httpheader <- c(httpheader, "content-type" = "application/json")
  req <- list(url = manta_call, method = "POST", headers = httpheader)
  bunyanLog.debug(msg ="curlPerform POST", req = req, version = manta_globals$RSDK_version)
  body <- toJSON(job)
  curl <- getCurlHandle()
  h = basicTextGatherer()

  
  h$reset()
  tryCatch(curlPerform(url = manta_call,
                           curl = curl,
                           httpheader = httpheader,
                           postfields = body,
                           writefunction = h$update,
                           header = TRUE,
                           post = 1L,
                           verbose = verbose,    
                           .encoding = 'UTF-8'),
                    COULDNT_RESOLVE_HOST = function(e) {
                           msg <- paste("mantaJob.launch Cannot Resolve Manta Host at\n ",
                           manta_globals$manta_url ,"\n",sep="")
                           bunyanLog.error(msg = msg, version = manta_globals$RSDK_version)
                           stop(msg)
                           },
                    error = function(e) {
                           msg <- paste("mantaJob.launch POST HTTP or RCURL error: ", e$message, "\n", sep="")
                           bunyanLog.error(msg = msg, version = manta_globals$RSDK_version)
                           stop(msg)
                          }
                    )


  ## 201 return code.
  reply <- h$value()

  replysplit <- function(reply, returncode, code) {
    split_reply <- strsplit(reply, split = "\r\n\r\n")
    header <- split_reply[[1]][1]
    body <- split_reply[[1]][-1] # in R this removes the first element in the array
    header_lines <- strsplit(header, split= "\r\n")
    no_body <- FALSE
    if (!length(body)==0) {
     body_lines <- strsplit(body[[1]], split = "\n")
    } else {
     body_lines <- c("")
     no_body <- TRUE
    }
    returned_code = ""
    returned_string <- header_lines[[1]][ charmatch("HTTP", header_lines[[1]]) ] 
    returned_code <- strsplit(returned_string, split=" ")[[1]][2]
    res <- list(statusCode = returned_code, headers = header_lines)
    bunyanLog.debug(msg ="mantaJob.launch server return", res = res, version = manta_globals$version) 
    # Server Error Responses
    if (as.integer(returned_code) >= 400) {
      if (isValidJSON(body_lines[[1]], asText = TRUE)) {
        values <- fromJSON(body_lines[[1]])
        # this checks the error strings to see if it is on the list...
        if (sum(charmatch(values, manta_globals$manta_error_classes, nomatch = 0)) > 0) {
          msg <- "mantaJob.launch Manta Service Error: "
        } else {
          msg <- "mantaJob.launch Unknown Error Class: "
        }
        # It was valid JSON, so show it as the return error message
        msg <- paste(msg, values,"\n",sep="")
      } else {  
        # not valid JSON returned, just return the error code...
        msg <- paste("mantaJob.launch Unrecognized - Server Error Code: ", returned_string, "\n", sep=" ")         
      } 
      bunyanLog.error(msg = msg, version = manta_globals$version) 
      stop(msg)
    }
    
    if (code == FALSE) {
      location_string <- ""
      location_string <- header_lines[[1]][grepl("Location", header_lines[[1]], ignore.case = TRUE) ]
      job_string <- strsplit(location_string, split=" ")[[1]][2]
      job_split <- strsplit(job_string, "/")
      jobid <- job_split[[1]][length(job_split[[1]])]
      return(jobid)
    } else {
      return(returned_code)
    }
  }

  ## looking for:   Location: ~~/jobs/a62ba79e-4d5b-4773-bff9-ecae0fe30dfa

  ## cat the jobid before sending inputs
  jobid <- replysplit(reply, returncode = "201", code = FALSE)

  msg <- paste("Job ID: ", jobid, "\n", sep="")
  bunyanLog.info(msg)
  cat(msg)

## Process the inputs...  THIS WILL NEED TO LOOP FOR BATCHES....

  manta_call <- paste(manta_globals$manta_url,"/",manta_globals$manta_user,"/jobs/", jobid, "/live/in",  sep="")
  httpheader <- mantaGenHeaders()
  httpheader <- c(httpheader, "content-type" = "text/plain")
  req <- list(url = manta_call, method = "POST", headers = httpheader)
  bunyanLog.debug(msg ="curlPerform POST", req = req, version = manta_globals$RSDK_version)
  body <- paste(inputs, "\n", collapse = "", sep="")

  h$reset()
  tryCatch(curlPerform(url = manta_call,
                           curl = curl,
                           httpheader = httpheader,
                           postfields = body,
                           writefunction = h$update,
                           header = TRUE,
                           post = 1L,
                           verbose = verbose,    
                           .encoding = 'UTF-8'),
                    COULDNT_RESOLVE_HOST = function(e) {
                           msg <- paste("mantaJob.launch Inputs Cannot Resolve Manta Host at\n ",
                           manta_globals$manta_url ,"\n",sep="")
                           bunyanLog.error(msg = msg, version = manta_globals$RSDK_version)
                           stop(msg)
                           },
                    error = function(e) {
                           msg <- paste("mantaJob.launch Inputs POST HTTP or RCURL error: ", e$message, "\n", sep="")
                           bunyanLog.error(msg = msg, version = manta_globals$RSDK_version)
                           stop(msg)
                          }
                    )


  ## 204 return code.
  reply <- h$value()
  returned_code <- replysplit(reply, returncode = "204", code = TRUE)
  cat(paste("Input code: ",returned_code, "\n", sep=""))
  if (returned_code == "204") {
   msg <- paste("Added ", length(inputs), " inputs to Job ", jobid, "\n",sep="")
  }

  
## End the inputs

  manta_call <- paste(manta_globals$manta_url,"/",manta_globals$manta_user,"/jobs/", jobid, "/live/in/end",  sep="")
  httpheader <- mantaGenHeaders()
  httpheader <- c(httpheader, "content-type" = "text/plain")
  req <- list(url = manta_call, method = "POST", headers = httpheader)
  bunyanLog.debug(msg ="curlPerform POST", req = req, version = manta_globals$RSDK_version)
  body <- "\n"

  h$reset()
  tryCatch(curlPerform(url = manta_call,
                           curl = curl,
                           httpheader = httpheader,
                           postfields = body,
                           writefunction = h$update,
                           header = TRUE,
                           post = 1L,
                           verbose = verbose,    
                           .encoding = 'UTF-8'),
                    COULDNT_RESOLVE_HOST = function(e) {
                           msg <- paste("mantaJob.launch Inputs Cannot Resolve Manta Host at\n ",
                           manta_globals$manta_url ,"\n",sep="")
                           bunyanLog.error(msg = msg, version = manta_globals$RSDK_version)
                           stop(msg)
                           },
                    error = function(e) {
                           msg <- paste("mantaJob.launch Inputs POST HTTP or RCURL error: ", e$message, "\n", sep="")
                           bunyanLog.error(msg = msg, version = manta_globals$RSDK_version)
                           stop(msg)
                          }
                    )


  ## 204 return code.  OR #202
  reply <- h$value()
  returned_code <- replysplit(reply, returncode = "204", code = TRUE)
  cat(paste("End Input Code:", returned_code, "\n", sep=""))

}
