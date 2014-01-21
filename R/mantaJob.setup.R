# Roxygen Comments mantaJob.setup
#' Constructor for R format Manta Job including \code{name}, and UNIX command line 
#' tasks as defined by
#' \code{mantaMap}, and/or \code{mantaReduce} functions.
#'
#' Function to construct R structure for \code{\link{mantaJob.launch}}. Specify a \code{name} 
#' for the Manta job and tasks to execute via one or more calls to 
#' \code{\link{mantaMap}} and/or \code{\link{mantaReduce}} which define and 
#' parameterize each task.
#'
#' mantaJob.setup combines multiple tasks into a job pipeline structure for 
#' \code{\link{mantaJob.launch}}. 
#' See \code{\link{mantaMap}} and \code{\link{mantaReduce}} for parameter details. 
#' Their \code{exec} parameter must be a valid generic UNIX command line, not an R function.
#'
#' @param name character, optional.
#' 
#' @param ... One or more \code{mantaMap} \code{mantaReduce} functions with arguments 
#' listed in order of task execution.
#' 
#' @keywords Manta
#'
#' @family mantaJobs
#'
#' @examples
#' \dontrun{
#' ##  Map/Reduce Unix Word Count Job description
#' job <- mantaJob.setup("Word Count", 
#'   mantaMap("wc"), 
#'   mantaReduce("awk '\{ l += $1; w += $2; c += $3 \} END \{ print l, w, c \}'"))
#' ## Launch the Job with some text files as input:
#' inputs <- mantaLs.paths("~~/stor/shakespeare", grepfor = "[.]txt")
#' mantaJob.launch(inputs, job)
#' ## Check output
#' mantaJob.outputs()
#' ## Check errors
#' mantaJob.errors()
#' }
#'
#' @export
mantaJob.setup <- 
function(name, ... )  {
  # If this is the first export function called in the library
  if (manta_globals$manta_initialized == FALSE) {
    mantaInitialize(useEnv = TRUE)
  }
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
