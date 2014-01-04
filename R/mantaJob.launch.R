# Roxygen Comments mantaJob.launch
#' Submits list of input files and R format Manta Job specification, runs job
#' optionally polls job status. Returns job identifier.
#'
#' Job is created by mantaJob.setup() and tasks as defined therein by
#' mantaMap(), and/or mantaReduce() functions. Note that Manta tasks
#' are UNIX shell commands, not native R commands. 
#'
#' Example - Manta Map/Reduce UNIX Word Count:
#'
#' status <- mantaJob.launch( 
#'    inputs = mantaLs.paths("~~/public/shakespeare", grepfor = "[.]txt"), 
#'    job = mantaJob.setup(
#'             name = "word count",
#'             mantaMap("wc"),
#'             mantaReduce("awk '{ l += $1; w += $2; c += $3 } END { print l, w, c }'")
#'          )
#' )
#'
#' Getting Job Results:
#"
#' mantaJob.status(jobid) # check to see if job is complete, as JSON information
#' mantajob.done(jobid)   # returns logical job done (TRUE/FALSE)
#' mantaJob.output(jobid) # retrieve list of paths to Manta output objects
#' mantaJob.errors(jobid) # retrieve JSON formatted job error information
#
#' @param inputs, optional. List of inputs as a vector of character, each containing 
#' valid paths to Manta objects that are the intended job input files. You may use
#' output from mantaFind() or mantaLs.paths() here. If you have no inputs, your
#' initial Job task must be a mantaReduce() step.
#'
#' @param job, required. The R job structure as created with mantaJob.Setup() and
#' Map and Reduce job tasks as defined therein by one or more mantaMap() and/or
#' mantaReduce() steps. 
#' More information and parameters are explained in the help for these three functions. 
#'
#' @param batchsize numeric. Maximum number of input object paths to upload
#' in one batch to the running job. Default is 500.
#' 
#' @param watch logical. Call mantaJob.done() in polling mode, sleeping for for sleep
#' seconds up to the duration of the watchtimeout value in seconds. This causes
#' the function to wait until the job is done to return, or timed out. Timeout
#' does not imply job success or failure.
#'
#' @param sleep integer. Number of seconds to wait between status requests in polling mode
#' when watch is set to TRUE. Default is 30 seconds.
#'
#' @param watchtimeout integer. Number of seconds after which polling ends. Passed
#' to mantaJob.done() when watch is set to TRUE. Default is 10 minutes (600 seconds)..
#' If watchtimeout is exceeded, it simply means the job is still running or queued on
#' Manta. mantaJob.done() or mantaJob.status() can be called after.
#'
#' @param silent logical. Supress console messages, does not affect verbose setting. 
#"
#' @param verbose logical optional. Passed to RCURL to reveal HTTP communication.
#'
#' @return Returns a Manta Status JSON structure. The Manta Job identifier is the "id":
#' field - like this "70c30bab-873b-66da-ebc8-ced12bd35ac4". This value is the jobid 
#' parameter to be used used by other mantaJob functions
#' for information, error and output retrieval as a lookup key. This key can
#' also be used by Node.js Manta command-line mjob commands. 
#'
#' @keywords Manta, manta
#'
#' @export
mantaJob.launch <-
function(inputs, job, batchsize = 500,  watch = TRUE, sleep = 30, watchtimeout = 600, silent = FALSE, verbose = FALSE) {

  if (missing(job)) 
    stop("mantaJob.launch - no job specified")

  noinput <- FALSE
  if (missing(inputs)) 
    noinput <- TRUE  

  #Syntactic NaCl
  if (is.null(job$token)) {
    stop("mantaJob.launch - Token missing error:  Job values passed in via job paramater must be output from mantaJob.setup")
  } else {
     if (job$token != "FEEDB0B0") {
       stop("mantaJob.launch - Token mismatch error:  Job values passed in via job paramater must be output from mantaJob.setup")
     }
  }

  job <- job$job

  info <- !silent

  if (isValidJSON(toJSON(job), asText = TRUE) == FALSE) 
    stop("mantaJob.launch - Job specified is not valid as JSON.")

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

  curlpost <- 
  function(url, curl, httpheader, body, verbose) 
  {
    h = basicTextGatherer()
    h$reset()
    tryCatch(curlPerform(url = url,
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
    reply <- h$value()
    return(reply)
  }

    
  replysplit <- 
  function(reply, returncode, code) {
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
        msg <- paste(msg, returned_code, values,"\n",sep="")
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

  reply <- curlpost(url = manta_call, curl = curl, httpheader = httpheader, body = body, verbose = verbose)
  jobid <- replysplit(reply, returncode = "201", code = FALSE)
  msg <- paste("Job ID: ", jobid, "\n", sep="")
  bunyanLog.info(msg)
  if (info == TRUE) {
    cat(msg)
  }

  input_sent <- 0
  if (noinput == FALSE) {
    ## Process the supplied inputs
    manta_call <- paste(manta_globals$manta_url,"/",manta_globals$manta_user,"/jobs/", jobid, "/live/in",  sep="")

    input_num <- length(inputs)
    batches <- ceiling(input_num / batchsize)
    for (i in 1:batches) {
      b <- 1 + (i - 1)*batchsize
      e <- i * batchsize
      input_batch <- inputs[b:e]
      input_batch <- input_batch[!is.na(input_batch)]
      httpheader <- mantaGenHeaders()
      httpheader <- c(httpheader, "content-type" = "text/plain")
      req <- list(url = manta_call, method = "POST", headers = httpheader)
      bunyanLog.debug(msg ="curlPerform POST", req = req, version = manta_globals$RSDK_version)
      body <- paste(input_batch, "\n", collapse = "", sep="")
      reply <- curlpost(url = manta_call, curl = curl, httpheader = httpheader, body = body,  verbose = verbose)
      ## 204 return code - is sometimes #202 
      returned_code <- replysplit(reply, returncode = "204", code = TRUE)
      # cat(paste("Input code: ",returned_code, "\n", sep=""))
      if ((returned_code == "204") || (returned_code == "202")) {
        msg <- paste(length(input_batch), " inputs added to job ", jobid, "\n",sep="")
        input_sent <- input_sent + length(input_batch)
        bunyanLog.info(msg)
        if (info == TRUE) {
          cat(msg)
        }
      }  else {
         # Something bad happened. Try to cancel the job and stop.
         ## Perhaps this could be a retry, but let the user relaunch the job. 
         msg <- paste("mantaJob.launch Error: ", length(input_batch), " inputs - were NOT added to job, Trying to cancel job", jobid, "\n",sep="")
         bunyanLog.error(msg)
         cat(msg)
         if (mantaJob.cancel(jobid) == FALSE) {
           msg <- paste("mantaJob.launch failed to send all inputs, could not cancel jobid: ", jobid, "\n", 
                        "Use mantaJob.cancel(", jobid, ") to manually cancel failed job." , sep="")
         } else {
           msg <- paste("mantaJob.launch error sending inputs. Job ", jobid,  "was cancelled.", sep = "")
         }
         bunyanLog.info(msg)
         stop(msg)
      }
    }
  }  

  ## End the inputs

  manta_call <- paste(manta_globals$manta_url,"/",manta_globals$manta_user,"/jobs/", jobid, "/live/in/end",  sep="")
  httpheader <- mantaGenHeaders()
  httpheader <- c(httpheader, "content-type" = "text/plain")
  req <- list(url = manta_call, method = "POST", headers = httpheader)
  bunyanLog.debug(msg ="curlPerform POST", req = req, version = manta_globals$RSDK_version)
  body <- "\n"

  reply <- curlpost(url = manta_call, curl = curl, httpheader = httpheader, body = body,  verbose = verbose)
  ## 204 return code.  OR #202
  returned_code <- replysplit(reply, returncode = "204", code = TRUE)
  if ((returned_code == "204") || (returned_code == "202")) {
      msg <- paste("Total of ", input_sent, " inputs added to job ", jobid, "\n\n",sep="")
      bunyanLog.info(msg)
      if (info == TRUE) { 
        cat(msg)
      }
  }
  
  ## Poll till done or timeout..

  if (watch == TRUE) {
    done <- mantaJob.done(jobid, poll = TRUE, sleep = sleep, timeout = watchtimeout, silent = silent)
  } 

  ## return the status / pretty print JSON

  status <- mantaJob.status(jobid, readable = FALSE)
  
  if (info == TRUE) {
    cat("\n")
    cat(paste(toJSON(fromJSON(status), pretty=TRUE), "\n", sep=""))
    cat("\n")
  }     

  return(status)
}
