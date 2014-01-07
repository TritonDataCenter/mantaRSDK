# Roxygen Comments mantaJob.setup
#' Constructor for R format Manta Job including name, and tasks as defined by
#' mantaMap(), and/or mantaReduce() functions. Does not specify job input.
#'
#' Function to construct R structure for mantaJob.launch. Specify a name 
#' for the Manta job and tasks to execute via one or more calls to 
#' mantaMap() and/or mantaReduce() which define and parameterize each task.
#' mantaJob.setup combines tasks into a job pipeline structure for mantaJob.launch
#' See mantaMap() and mantaReduce() for details of their arguments. 
#' The exec argument must be a valid generic UNIX command line, not an R function.
#'
#' Example - Map/Reduce Unix Word Count
#'
#' job <- mantaJob.setup("word count", mantaMap("wc"), 
#' mantaReduce("awk '\{ l += $1; w += $2; c += $3 \} END \{ print l, w, c \}'"))
#' 
#' mantaJob.launch(inputs = mantaLs.paths("~~/public/shakespeare", grepfor = "[.]txt"), job)
#'
#
#' @param name character, optional.
#' 
#' @param ... One or more mantaMap() mantaReduce() functions with arguments in order of task execution
#' 
#' @keywords Manta, manta
#'
#' @export
mantaJob.setup <- 
function(name, ... )  {
  phases <- list(...)
  job <- list()
  if (missing(name)) {
    job <- c(list(phases = phases))
  } else {
    job <- c(name = name, list(phases = phases))
  }
  job_json <- toJSON(job)
  if (isValidJSON(job_json, asText = TRUE)) {
   return(list(job = job, token = "FEEDB0B0"))
  } else {
    msg <- paste("mantaJob.setup - Malformed JSON, check arguments: ... must be return values from mantaMap or mantaReduce")
    bunyanLog.error(msg)
    return()
  }
}
